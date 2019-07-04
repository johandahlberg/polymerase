package scilifelab.polymerase

import java.lang.{Short => JavaShort}
import scala.io.Source
import java.io.File
import java.io.EOFException
import java.nio.charset.Charset
import scala.io.StdIn
import java.nio.ByteBuffer

case object DNACodec {

  val charset = Charset.forName("UTF-8")
  type Nucleotide = Char

  private val decodingTable: Map[Nucleotide, String] =
    Map('G' -> "00", 'A' -> "01", 'T' -> "10", 'C' -> "11")
  private val encodingTable = decodingTable.map { case (k, v) => (v, k) }

  def encode(data: String): Iterable[Nucleotide] = {
    encode(data.getBytes(charset))
  }

  def encode(data: Iterable[Byte]): Iterable[Nucleotide] = {
    data.map(encode(_)).flatten
  }

  def encode(data: Byte): Iterable[Nucleotide] = {
    val binaryStringData =
      String
        .format("%8s", Integer.toBinaryString(data & 0xFF))
        .replace(' ', '0')
    binaryStringData
      .toCharArray()
      .grouped(2)
      .map { bits =>
        encodingTable(bits.mkString)
      }
      .toIterable
  }

  def decode(data: Iterable[Nucleotide]): Iterable[Byte] = {
    data
      .grouped(4)
      .map { groupOfFourBases =>
        val byteAsBinaryString = groupOfFourBases.map { base =>
          decodingTable(base)
        }.mkString
        val resultingByte = JavaShort.parseShort(byteAsBinaryString, 2).toByte
        resultingByte
      }
      .toIterable
  }

  def decodeString(data: Iterable[Nucleotide]): String = {
    val byteDecoded = decode(data)
    charset.decode(ByteBuffer.wrap(byteDecoded.toArray)).toString()
  }
}

object PolymeraseEncoder extends App {
  var line = ""
  while ({ line = StdIn.readLine(); line != null }) {
    print(DNACodec.encode(line.toArray.map(_.toByte)).mkString)
  }
}

object PolymeraseDecoded extends App {
  var line = ""
  while ({ line = StdIn.readLine(); line != null }) {
    print(DNACodec.decode(line).map(_.toChar).mkString)
  }

}
