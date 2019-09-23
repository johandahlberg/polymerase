package se.scilifelab.polymerase

import java.nio.charset.Charset
import java.lang.{Short => JavaShort}
import java.nio.ByteBuffer

case object DNACodec {

  val charset = Charset.forName("UTF-8")

  private val decodingTable: Map[Nucleotide, String] =
    Map('G' -> "00", 'A' -> "01", 'T' -> "10", 'C' -> "11")
  private val encodingTable = decodingTable.map { case (k, v) => (v, k) }
  private val encodeCache =
    new collection.mutable.WeakHashMap[Byte, Seq[Nucleotide]]
  private val decodeCache = new collection.mutable.WeakHashMap[String, Byte]

  private def groupString(str: String, len: Int): Seq[String] = {
    for (p <- 0 until str.length() by len)
      yield str.substring(p, p + len)
  }

  private def byteToBinaryString(byte: Byte): String = {
    String
      .format("%8s", Integer.toBinaryString(byte & 0xFF))
      .replace(' ', '0')
  }

  def encode(data: String): Iterator[Nucleotide] = {
    encode(data.getBytes(charset).toIterator)
  }

  def encode(data: Iterator[Byte]): Iterator[Nucleotide] = {
    data.map(encode(_)).flatten
  }

  def encode(data: Array[Int]): Array[Nucleotide] = {
    data.map(int => encode(int)).flatten
  }

  // TODO Encode that we only accept integers in the range 0 - 255
  def encodeBlocks(data: Iterator[Array[Int]]): Iterator[Array[Nucleotide]] = {
    CodecUtils.createDataBlock(data).map(datum => encode(datum))
  }

  def encode(data: Int): Seq[Nucleotide] = {
    val integerByteString = byteToBinaryString(data.toByte)
    encodeBinaryString(integerByteString)
  }

  def encode(data: Byte): Seq[Nucleotide] = {
    val integerByteString = byteToBinaryString(data)
    encodeBinaryString(integerByteString)
  }

  def encodeBinaryString(binaryStringData: String): Seq[Nucleotide] = {
    val nuc = groupString(binaryStringData, 2).map { bits =>
      encodingTable.getOrElse(bits, {
        throw new NoSuchElementException(
          f"There was a problem encoding bit, bits: ${bits.mkString}"
        )
      })
    }
    nuc
  }

  def decodeBlocks(data: Iterator[Array[Nucleotide]]): Iterator[Byte] = {
    val decodedBlocks = data
      .map { datum =>
        decode(datum.iterator).map(_ & 0xff)
      }
      .map { datum =>
        CodecUtils
          .deconstructDataBlock(datum.toArray)
      }
    CodecUtils.sortIndexAndDataOutput(decodedBlocks)
  }

  def decode(data: Iterator[Nucleotide]): Iterator[Byte] = {

    def fourBasesToByte(groupOfFourBases: Seq[Nucleotide]): Byte = {

      val byteAsBinaryString = groupOfFourBases.map { base =>
        decodingTable.getOrElse(base, {
          throw new NoSuchElementException(
            f"There was a problem decoding base: ${base}. Perhaps this is not a DNA Sequence?"
          )
        })
      }.mkString
      val resultingByte = JavaShort.parseShort(byteAsBinaryString, 2).toByte

      decodeCache.update(groupOfFourBases.mkString, resultingByte)
      resultingByte
    }

    data
      .grouped(4)
      .map { groupOfFourBases =>
        decodeCache.getOrElse(groupOfFourBases.mkString, {
          fourBasesToByte(groupOfFourBases)
        })
      }
  }

  def decodeString(data: Iterator[Nucleotide]): String = {
    val byteDecoded = decode(data)
    charset.decode(ByteBuffer.wrap(byteDecoded.toArray)).toString()
  }
}
