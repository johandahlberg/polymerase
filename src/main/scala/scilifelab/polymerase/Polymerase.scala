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

case object DNACodec {

  val charset = Charset.forName("UTF-8")
  type Nucleotide = Char

  private val decodingTable: Map[Nucleotide, String] =
    Map('G' -> "00", 'A' -> "01", 'T' -> "10", 'C' -> "11")
  private val encodingTable = decodingTable.map { case (k, v) => (v, k) }

  private def groupString(str: String, len: Int): Seq[String] = {
    (for (p <- 0 until str.length() by len)
      yield str.substring(p, p + len))
  }

  def encode(data: String): Iterator[Nucleotide] = {
    encode(data.getBytes(charset).toIterator)
  }

  def encode(data: Iterator[Byte]): Iterator[Nucleotide] = {
    data.map(encode(_)).flatten
  }

  def encode(data: Byte): Iterator[Nucleotide] = {
    val binaryStringData =
      String
        .format("%8s", Integer.toBinaryString(data & 0xFF))
        .replace(' ', '0')
    groupString(binaryStringData, 2).map { bits =>
      encodingTable.getOrElse(bits, {
        throw new NoSuchElementException(
          f"There was a problem encoding bit, bits: ${bits.mkString}"
        )
      })
    }.toIterator
  }

  def decode(data: Iterator[Nucleotide]): Iterator[Byte] = {
    data
      .grouped(4)
      .map { groupOfFourBases =>
        val byteAsBinaryString = groupOfFourBases.map { base =>
          decodingTable.getOrElse(base, {
            throw new NoSuchElementException(
              f"There was a problem decoding base: ${base}"
            )
          })
        }.mkString
        val resultingByte = JavaShort.parseShort(byteAsBinaryString, 2).toByte
        resultingByte
      }
  }

  def decodeString(data: Iterator[Nucleotide]): String = {
    val byteDecoded = decode(data)
    charset.decode(ByteBuffer.wrap(byteDecoded.toArray)).toString()
  }
}

object PolymeraseEncoder extends App {

  val input = new DataInputStream(new BufferedInputStream(System.in))
  val output = new PrintWriter(System.out)
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
  val input = new BufferedInputStream(System.in)
  val output = new DataOutputStream(System.out)
  for { byte <- DNACodec.decode(Source.fromInputStream(input)) } {
    output.write(byte)
  }
  input.close()
  output.close()
}
