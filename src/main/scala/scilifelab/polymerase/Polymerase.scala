package scilifelab.polymerase

import java.lang.{Short => JavaShort}
import scala.io.Source
import java.io.File
import java.io.EOFException
import java.nio.charset.Charset
import scala.io.StdIn
import java.nio.ByteBuffer
import java.nio.file.Files
import java.io.BufferedOutputStream
import java.io.BufferedInputStream
import java.io.FileOutputStream
import java.io.OutputStream
import java.io.PrintStream
import java.io.DataOutputStream
import java.io.DataInputStream
import java.io.PrintWriter
import java.io.FileInputStream

case object DNACodec {

  val charset = Charset.forName("UTF-8")
  type Nucleotide = Char

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

  def encode(data: String): Iterator[Nucleotide] = {
    encode(data.getBytes(charset).toIterator)
  }

  def encode(data: Iterator[Byte]): Iterator[Nucleotide] = {
    data.map(encode(_)).flatten
  }

  def encode(data: Byte): Seq[Nucleotide] = {
    encodeCache.getOrElse(
      data, {
        val binaryStringData =
          String
            .format("%8s", Integer.toBinaryString(data & 0xFF))
            .replace(' ', '0')
        val nuc = groupString(binaryStringData, 2).map { bits =>
          encodingTable.getOrElse(bits, {
            throw new NoSuchElementException(
              f"There was a problem encoding bit, bits: ${bits.mkString}"
            )
          })
        }
        encodeCache.update(data, nuc)
        nuc
      }
    )
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

object PolymeraseEncoder extends App {

  val input = new DataInputStream(new BufferedInputStream(System.in))
  val output = new PrintWriter(new BufferedOutputStream(System.out))
  try {
    while (true) {
      val data = input.readByte()
      output.write(DNACodec.encode(data).toArray)
    }
  } catch {
    case e: EOFException =>
  } finally {
    input.close()
    output.close()
  }
}

object PolymeraseDecoded extends App {
  val input = System.in
  val output = new DataOutputStream(new BufferedOutputStream(System.out))

  for {
    byte <- DNACodec.decode(Source.fromInputStream(input))
  } {
    output.write(byte)
  }

  input.close()
  output.close()
}
