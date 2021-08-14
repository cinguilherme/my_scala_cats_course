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

object ByteCodec {

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

  implicit def optionCodec[A](implicit encA: ByteCodec[A]): ByteCodec[Option[A]] =
    new ByteCodec[Option[A]] {
      override def encode(a: Option[A]) = {
        a match {
          case Some(v) => encA.encode(v)
          case None => Array[Byte]()
        }
      }

      override def decode(a: Array[Byte]) = Option(encA.decode(a))

    }

}



ByteCodec.IntByteCodec.encode(10)
ByteCodec.StringByteCodec.encode("hello")
//ByteCodec.OptionStringCodec.encode(Some("hello"))
ByteCodec.IntByteCodec.encode(10)
//ByteCodec.OptionIntCodec.encode(Some(10))



import ByteCodec.StringByteCodec
ByteCodec.optionCodec.encode(Some("hello"))
