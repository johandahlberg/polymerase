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

  def encode(data: Iterable[Byte]): Iterable[Char] = {
    data.map(encode(_)).flatten
  }

  def encode(data: Byte): Iterable[Char] = {
    data.toBinaryString
      .map(_.toChar)
      .grouped(2)
      .map(bits => encodingTable(bits))
      .toIterable
  }

  def decode(data: Iterable[Char]): Iterable[Byte] = {
    data.grouped(4)
    ???
  }
}

//def decode(data: Char): Byte = {
//  val binaryString = decodingTable(data)
//  println(binaryString)
//  val nbr = JavaShort.parseShort(binaryString, 2)
//  println(nbr)
//  nbr.toByte
//}

object Polymerase extends App {

  val s = "Hello World!"
  println(f"String to write: $s")

  val encoded = DNACodec.encode(s.toStream.map(_.toByte))
  println(encoded)

}
