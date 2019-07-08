package scilifelab.polymerase

import java.nio.CharBuffer
import java.nio.ByteBuffer
import java.io.InputStream
import java.io.DataInputStream
import java.io.EOFException

case class DataContainer(
    index: Int,
    val data: Array[Char]
) extends Ordered[DataContainer] {

  val dataAsBytes =
    DNACodec.charset.encode(CharBuffer.wrap(data.toArray)).array()
  val bytesOfData: Int = dataAsBytes.length

  import scala.math.Ordered.orderingToOrdered
  def compare(that: DataContainer): Int = this.index compare that.index

  def toByteArray(): Array[Byte] = {
    ByteBuffer.allocate(4).putInt(index).array() ++
      ByteBuffer.allocate(4).putInt(bytesOfData).array() ++
      dataAsBytes
  }
}

case object DataContainer {
  def fromByteArray(array: Array[Byte]): DataContainer = {
    val buffer = ByteBuffer.wrap(array)
    val index = buffer.getInt()
    val bytesOfData = buffer.getInt()
    val dataAsBytes: Array[Byte] = Array.fill(bytesOfData)(0)
    buffer.get(dataAsBytes)
    val dataAsChars =
      DNACodec.charset.decode(ByteBuffer.wrap(dataAsBytes)).array()
    DataContainer(index = index, data = dataAsChars)
  }

  def fromInputStream(
      input: DataInputStream
  ): DataContainer = {

    val index = input.readInt()
    val currentDataLength = input.readInt()

    val data: Array[Byte] = Array.fill(currentDataLength) { 0 }
    val bytesRead = input.read(data)

    if (bytesRead == -1) throw new EOFException

    val dataAsChars = DNACodec.charset.decode(ByteBuffer.wrap(data)).array()
    DataContainer(index, data = dataAsChars)
  }
}
