package scilifelab.polymerase

import java.lang.{Short => JavaShort}
import scala.io.Source
import java.io.File
import java.io.EOFException
import scala.io.StdIn

case object DNACodec {

  private val decodingTable =
    Map('G' -> "00", 'A' -> "01", 'T' -> "10", 'C' -> "11")
  private val encodingTable = decodingTable.map { case (k, v) => (v, k) }

  def encode(data: Iterable[Byte]): Iterable[Char] = {
    if (data.isEmpty) {
      println("Encoding data was empty")
      Iterable.empty
    } else {
      data.map(encode(_)).flatten
    }
  }

  def encode(data: Byte): Iterable[Char] = {
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

  def decode(data: Iterable[Char]): Iterable[Byte] = {
    if (data.isEmpty) {
      println("Decoding data was empty")
      Iterable.empty
    } else {
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
