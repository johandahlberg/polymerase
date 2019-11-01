package se.scilifelab.reedsolomon

import java.io.{
  BufferedInputStream,
  DataInputStream,
  PrintWriter,
  BufferedOutputStream,
  EOFException
}
import java.nio.charset.Charset
import java.nio.ByteBuffer

object Defaults {
  val messageSize = 223
  val dictonarySize = 255
}

object RSEncode extends App {
  val coder =
    ReedSolomonCoder(n = Defaults.dictonarySize, k = Defaults.messageSize)

  val input = new DataInputStream(new BufferedInputStream(System.in))
  val output = new BufferedOutputStream(System.out)

  var c = 0
  var sizeOfLastOutput = 0
  while (c >= 0) {
    val readArray = Array.fill(Defaults.messageSize - Integer.BYTES)(0.toByte)
    c = input.read(readArray)
    if (c > 0) {

      // Prepend each block with the size of the actual data
      val data =
        c +: readArray.map(x => x & (0xff)).map(_.toInt)

      val encodedMessage =
        coder.encode(data).map(_.toByte)

      output.write(encodedMessage)
    }
  }

  input.close()
  output.flush()
  output.close()

}

object RSDecode extends App {

  val coder =
    ReedSolomonCoder(n = Defaults.dictonarySize, k = Defaults.messageSize)

  val input = new DataInputStream(new BufferedInputStream(System.in))
  val output = new BufferedOutputStream(System.out)

  var c = 0
  while (c >= 0) {
    val readArray = Array.fill(Defaults.dictonarySize)(0.toByte)
    c = input.read(readArray)
    if (c > 0) {
      val data = readArray.map(x => x & (0xff)).map(_.toInt)
      val (res, _) = coder.decode(data)

      // Only pick up the original data, i.e. strip the length of the data block
      val length = res.head
      output.write(
        res.tail.take(length).map(_.toByte).toArray
      )
    }
  }

  input.close()
  output.flush()
  output.close()

}
