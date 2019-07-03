package scilifelab.polymerase
import java.io.FilterOutputStream
import java.io.OutputStream
import java.io.PrintWriter
import java.io.FileOutputStream
import java.io.FileInputStream
import scala.io.Source
import java.io.File
import java.io.InputStream
import java.io.FilterInputStream
import java.math.BigInteger

import java.lang.{Short => JavaShort}
import java.nio.ByteBuffer

case object DNACodec {

  private val decodingTable =
    Map('G' -> "00", 'A' -> "01", 'T' -> "10", 'C' -> "11")
  private val encodingTable = decodingTable.map { case (k, v) => (v, k) }

  def encode(b: Array[Byte]): Array[Byte] = {
    (for {
      byte <- b
      twobitsString <- byte.toBinaryString.grouped(2)
      twoBits <- encodingTable.get(twobitsString)
    } yield {
      twoBits
    }).map(_.toByte)
  }

  def decode(data: Char): Byte = {
    val binaryString = decodingTable(data)
    println(binaryString)
    val nbr = JavaShort.parseShort(binaryString, 2)
    println(nbr)
    nbr.toByte
  }
}

case class DNAOutputStream(outputStream: OutputStream)
    extends FilterOutputStream(outputStream) {

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    outputStream.write(DNACodec.encode(b), off, len)
  }
}

case class DNAInputStream(inputStream: InputStream)
    extends FilterInputStream(inputStream) {
  override def read(
      data: Array[Byte],
      originalOff: Int,
      len: Int
  ): Int = {

    // data needs to be manipulated, and the return of the function indicates how many
    // bytes were actually read...

    var off = originalOff

    println("Using specific read...")

    if (off < 0 || len < 0 || off + len > data.length) {
      throw new ArrayIndexOutOfBoundsException()
    }

    if (len == 0) {
      return 0
    }

    Range(0, len)
      .map { _ =>
        read()
      }
      .filter { ch =>
        ch >= 0
      }
      .foreach { ch =>
        off += 1
        data(off) = DNACodec.decode(ch.toChar)
        println(data.mkString(","))
      }

    if (off == originalOff) {
      -1
    } else {
      off - originalOff
    }
  }
}

object Polymerase extends App {

  val s = "Hello World!"
  println(f"String to write: $s")

  // Write the data
  println("Write data")
  val fileOutputStream = new FileOutputStream(new File("foo"))
  val dnaOutputStream = new DNAOutputStream(fileOutputStream)
  val writer = new PrintWriter(dnaOutputStream)

  writer.write(s)
  writer.flush()
  fileOutputStream.close()
  dnaOutputStream.close()

  println("Read data back")
  val fileInputStream = new FileInputStream(new File("foo"))
  def dnaInputStream = {
    if (true)
      new DNAInputStream(fileInputStream)
    else
      fileInputStream
  }

  println("Data: ")
  println("----------------------")
  for { line <- Source.fromInputStream(dnaInputStream).getLines } {
    println(f"$line\t${line.length()}")
  }
  println("----------------------")

  println("Closing streams")
  fileInputStream.close()
  dnaInputStream.close()
}
