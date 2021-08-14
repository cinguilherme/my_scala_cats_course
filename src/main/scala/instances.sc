import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

import java.nio.ByteBuffer

trait ByteEncodable[A] {
  def encode(a: A): Array[Byte]
}

trait ByteDecoder[A] {
  def decode(a: Array[Byte]): Option[A]
}

trait ByteCodec[A]
  extends ByteEncodable[A]
    with ByteDecoder[A] {
}


implicit object OptionStringCodec extends ByteCodec[Option[String]] {
  override def decode(a: Array[Byte]) = {
    Option((StringByteCodec.decode(a)))
  }

  override def encode(a: Option[String]) = {
    a match {
      case Some(value) => StringByteCodec.encode(value)
      case None => Array[Byte]()
    }
  }
}

implicit object StringByteCodec extends ByteCodec[String] {
  override def encode(a: String) = a.getBytes

  override def decode(a: Array[Byte]) = Option(a.toString)
}

implicit object IntByteCodec extends ByteCodec[Int] {
  override def encode(a: Int) = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(a)
    bb.array()
  }

  override def decode(a: Array[Byte]) = {
    if (a.length != 4) None
    else {
      val bb = ByteBuffer.allocate(4)
      bb.put(a)
      bb.flip()
      Some(bb.getInt)
    }
  }
}



trait ByteCodecLaws[A] {
  def codec: ByteCodec[A]

  def isomorphism(a: A): Boolean =
    codec.decode(codec.encode(a)) == Option(a)
}

trait ByteCodecTest[A] extends Laws {

  def laws: ByteCodecLaws[A]

  def byteCodec(implicit arb: Arbitrary[A]): RuleSet =
    new DefaultRuleSet(
      name = "ByteCodec",
      parent = None,
      "isomorphism" -> forAll(laws.isomorphism _)
    )
}
//
//class ByteCodecSpec extends AnyFunSuite
//  with Configuration
//  with FunSuiteDiscipline {
//
//  checkAll("ByteCodec[Int]", ByteCodecTest[Int].byteCodec)
//  checkAll("ByteCodec[String]", ByteCodecTest[String].byteCodec)
//}

object ByteCodecTest {
  def apply[A](implicit bc: ByteCodec[A]): ByteCodecTest[A] = new ByteCodecTest[A] {
    override def laws = new ByteCodecLaws[A] {
      override def codec = bc
    }
  }
}

object ByteDecoder {
  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = {
    new ByteDecoder[A] {
      override def decode(bytes: Array[Byte]): Option[A] = f(bytes)
    }
  }

  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A] = ev
}

object ByteEncodable {
  def instance[A](f: A => Array[Byte]): ByteEncodable[A] = {
    (a: A) => f(a)
  }
}

object StringByteCodecLaws extends ByteCodecLaws[String] {
  override def codec = StringByteCodec
}

object OptionStringCodecLaw extends ByteCodecLaws[Option[String]] {
  override def codec = OptionStringCodec
}

object IntByteCodecLaws extends ByteCodecLaws[Int] {
  override def codec = IntByteCodec
}

object StringByteCodecTests extends ByteCodecTest[String] {
  override def laws = StringByteCodecLaws
}

object OptionStringByteCodecTests extends ByteCodecTest[Option[String]] {
  override def laws = OptionStringCodecLaw
}

object IntByteCodecTests extends ByteCodecTest[Int] {
  override def laws = IntByteCodecLaws
}

OptionStringCodec.encode(Option("hello"))