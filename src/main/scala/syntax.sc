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

implicit class ByteCodecOps[A](val a: A) extends AnyVal {
  def encode(implicit enc: ByteCodec[A]): Array[Byte] = {
    enc.encode(a)
  }
}

implicit class ByteCodecOpDecode[A](a: Array[Byte]) {
  def decode(implicit enc: ByteCodec[A]): Option[A] = {
    enc.decode(a)
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


IntByteCodec.encode(10)

10.encode
"hello".encode
StringByteCodec.encode("hello")

Array[Byte](0,0,0,10).decode(IntByteCodec)
Array[Byte](104, 101, 108, 108, 111).decode(StringByteCodec)