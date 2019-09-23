package se.scilifelab.polymerase
import java.nio.ByteBuffer

object CodecUtils {

  def createDataBlock(data: Iterator[Array[Int]]): Iterator[Array[Int]] = {
    data.zipWithIndex
      .map {
        case (x, i) =>
          val index =
            ByteBuffer.allocate(4).putInt(i).array().map(_.toInt)
          val dataLength = x.length + index.length
          val dataAsBytes = (index ++ x).map(_ & 0xff)
          (dataLength +: dataAsBytes).toArray
      }
  }

  def deconstructDataBlock(data: Array[Int]): (Int, Array[Byte]) = {
    val length = data.head
    val index = ByteBuffer.wrap(data.tail.take(4).map(_.toByte)).getInt()
    val pck = data.drop(5).take(length).map(_.toByte)
    (index, pck)
  }
}
