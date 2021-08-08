import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait ByteEncodable[A] {
  def encode(a: A): Array[Byte]
}

case class Switch(isOn: Boolean)
object Switch {
  implicit object SwitchByteEncoder extends ByteEncodable[Switch] {
    override def encode(a: Switch) = {
      a match {
        case Switch(true) => Array('1'.toByte)
        case _ => Array('0'.toByte)
      }
    }
  }
}

object ByteEncodable {
  implicit object IntEnc extends ByteEncodable[Int] {
    override def encode(a: Int) = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(a)
      bb.array()
    }
  }

  implicit object StringEnc extends ByteEncodable[String] {
    override def encode(a: String) = a.getBytes
  }
}

trait Channel {
  def write[A](obj: A) (implicit enc: ByteEncodable[A]): Unit
}

object FileChannel extends Channel {
  override def write[A](obj: A) (implicit  enc: ByteEncodable[A]): Unit = {

    val bytes: Array[Byte] = enc.encode(obj)

    Using(new FileOutputStream("test")) { os =>
      os.write(bytes)
      os.flush()
    }

  }
}

implicit object StringEnc3 extends ByteEncodable[String] {
  override def encode(a: String) = {
    a.getBytes.map(b => (b + 3).toByte)
  }
}

ByteEncodable.StringEnc.encode("hello")

FileChannel.write(10)(ByteEncodable.IntEnc)
FileChannel.write("Hello")
FileChannel.write("Hello")(StringEnc3)

FileChannel.write(Switch(true))
FileChannel.write(Switch(false))
