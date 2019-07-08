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

import scilifelab.polymerase._
import scala.util.Random
import java.io.InputStream

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

object ErrorSimulator {
  private val probOfError = 0.01
  private val randomGenerator = new Random()
  private def addError: Boolean = randomGenerator.nextFloat < probOfError

  def addErrors(
      input: Iterator[Nucleotide]
  ): Iterator[Nucleotide] = {
    for { nuc <- input } yield {
      if (addError) {
        nucleotides(randomGenerator.nextInt(nucleotides.size))
      } else {
        nuc
      }
    }
  }
}

object PolymeraseEncode extends App {

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

object PolymeraseDecode extends App {
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

case class DataContainer(index: Int, currentDataLength: Int, data: Seq[Char])
    extends Ordered[DataContainer] {

  import scala.math.Ordered.orderingToOrdered
  def compare(that: DataContainer): Int = this.index compare that.index

}
case object DataContainer {
  val dataLength = 100
  val containerLength = 4 + 4 + dataLength
}

object PolymeraseSplit extends App {

  val input = System.in
  val output = new DataOutputStream(new BufferedOutputStream(System.out))

  def inputToDataContainers(input: InputStream): Iterator[DataContainer] = {
    Source
      .fromInputStream(input)
      .grouped(DataContainer.dataLength)
      .zipWithIndex
      .map {
        case (data, index) => {
          val currentDataLength = data.length
          if (currentDataLength < DataContainer.dataLength) {
            DataContainer(
              index,
              currentDataLength,
              data.padTo(DataContainer.dataLength, 'A')
            )
          } else {
            DataContainer(index, currentDataLength, data)
          }
        }
      }
  }

  for { dataContainer <- inputToDataContainers(input) } {
    output.write(dataContainer.index)
    output.write(dataContainer.currentDataLength)
    dataContainer.data.foreach(output.write(_))
  }

}

object PolymeraseJoin extends App {
  val input = new DataInputStream(new BufferedInputStream(System.in))
  val output = new PrintWriter(new BufferedOutputStream(System.out))

  val sortedInput = scala.collection.mutable.SortedSet[DataContainer]()

  try {
    while (true) {
      val byteArray: Array[Byte] = Array.fill(DataContainer.containerLength)(0)
      if (input.read(byteArray) == -1) throw new EOFException
      val dataContainer = DataContainer(
        index = byteArray(0),
        currentDataLength = byteArray(1),
        data = DNACodec.charset
          .decode(
            ByteBuffer.wrap(byteArray.slice(2, byteArray.length))
          )
          .array()
      )
      sortedInput(dataContainer) = true

    }
  } catch {
    case e: EOFException =>
  }

  for {
    elem <- sortedInput

  } {
    val data = elem.data.slice(0, elem.currentDataLength)
    output.write(data.toArray)
  }
  input.close()
  output.close()
}

object PolymeraseSimulateErrors extends App {
  val input = System.in
  val output = new DataOutputStream(new BufferedOutputStream(System.out))

  for {
    byte <- ErrorSimulator.addErrors(Source.fromInputStream(input))
  } {
    output.write(byte)
  }

  input.close()
  output.close()
}

object TestingReedSolomon extends App {
  import com.backblaze.erasure.ReedSolomon

  def printShards(shards: Array[Array[Byte]]) =
    shards.foreach { x =>
      println(
        x.mkString(" ")
      )
    }

  val codec = ReedSolomon.create(4, 2)
  val shards: Array[Array[Byte]] =
    Array(
      Array('A', 'B', 'C', 'D'),
      Array('E', 'G', 'G', 'H'),
      Array('I', 'J', 'K', 'L'),
      Array('M', 'N', 'O', 'P'),
      Array.fill(4)(0),
      Array.fill(4)(0)
    )

  println()
  println("Before encoding")

  printShards(shards)
  codec.encodeParity(shards, 0, 4)

  println()
  println("After encoding")
  printShards(shards)

  val present: Array[Boolean] = Array.fill(6) { true }
  codec.decodeMissing(shards, present, 0, 4)

  println()
  println("Decoded")
  printShards(shards)

}
