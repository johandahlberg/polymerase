package scilifelab.polymerase

import java.lang.{Short => JavaShort}

case object DNACodec {

  private val decodingTable =
    Map('G' -> "00", 'A' -> "01", 'T' -> "10", 'C' -> "11")
  private val encodingTable = decodingTable.map { case (k, v) => (v, k) }

  def encode(data: Iterable[Byte]): Iterable[Char] = {
    data.map(encode(_)).flatten
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

object Polymerase extends App {

  val s = "Hello World!"
  println(s"Encoding data: $s")
  println(
    s"Approx. size of data: ${s.length * 8} bits"
  )

  val encoded = DNACodec.encode(s.toStream.map(_.toByte)).toList
  println(s"Encoded as DNA: ${encoded.mkString}")
  println(s"Assumed size of DNA encoded data: ${encoded.length} bases")

  val decoded = DNACodec.decode(encoded)
  println(s"Decoded data: ${decoded.map(_.toChar).mkString}")

}
