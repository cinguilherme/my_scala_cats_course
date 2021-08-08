import java.nio.ByteBuffer
import scala.util.Try

trait ByteEncodable[A] {
  def encode(a: A): Array[Byte]
}

trait ByteDecoder[A] {
  def decode(a: Array[Byte]):Option[A]
}

trait ByteCodec[A]
  extends ByteEncodable[A]
    with ByteDecoder[A] {

}

trait ByteCodecLaws[A] {
  def codec : ByteCodec[A]

  def isomorphism(a: A):Boolean =
    codec.decode(codec.encode(a)) == Option(a)
}

trait ByteCodecTest[A] extends Laws {
  def laws : ByteCodecLaws[A]
  def byteCodec: RuleSet
}



override def isomorphism(a: String)(implicit codec: ByteCodec[String]) = {
  codec.decode(codec.encode(a)) == Some(a)
}

object ByteDecoder {
  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = {
    new ByteDecoder[A] {
      override def decode(bytes: Array[Byte]):Option[A] = f(bytes)
    }
  }
  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A] = ev
}

implicit object StringByteDecoder extends ByteDecoder[String] {
  override def decode(a: Array[Byte]):Option[String] =
    Try(new String(a)).toOption
}

object ByteEncodable {

  implicit val stringEnc = instance[String](_.getBytes)

  def instance[A](f: A => Array[Byte]): ByteEncodable[A] = {
    (a: A) => f(a)
  }
}

object ByteCodec {
  implicit val strCodec = new ByteCodec[String]{

    override def encode(a: String) = a.getBytes

    override def decode(a: Array[Byte]) =
      Try(new String(a)).toOption

  }
}
