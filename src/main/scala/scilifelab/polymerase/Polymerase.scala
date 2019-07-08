package scilifelab.polymerase

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
import java.io.InputStream
import java.nio.CharBuffer

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
  val dataByteLength: Int =
    DNACodec.charset
      .encode(CharBuffer.wrap(Array.fill(dataLength)('N')))
      .array()
      .length

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
    //println(s"index: ${dataContainer.index}")
    //println(s"currentDataLength: ${dataContainer.currentDataLength}")
    output.writeInt(dataContainer.index)
    output.writeInt(dataContainer.currentDataLength)
    output.write(
      DNACodec.charset
        .encode(CharBuffer.wrap(dataContainer.data.toArray))
        .array()
    )
    //println(dataContainer)
    //println(
    //  DNACodec.charset
    //    .encode(CharBuffer.wrap(dataContainer.data.toArray))
    //    .array()
    //    .mkString
    //)
  }

}

object PolymeraseJoin extends App {
  val input = new DataInputStream(new BufferedInputStream(System.in))
  val output = new PrintWriter(new BufferedOutputStream(System.out))

  val sortedInput = scala.collection.mutable.SortedSet[DataContainer]()

  try {
    var n = 0
    while (true) {
      n = n + 1

      //println(s"Added: $n elements")

      val index = input.readInt()
      //println(s"index is: ${index}")
      val currentDataLength = input.readInt()
      //println(s"current data length is: ${currentDataLength}")
      val data =
        Array
          .fill(DataContainer.dataByteLength)(
            DNACodec.charset.encode('N'.toString()).array()
          )
          .flatten
      //println(data.mkString)
      val bytesRead = input.read(data)
      //println(s"bytesRead: $bytesRead")
      if (bytesRead == -1) throw new EOFException

      //println(data.mkString)

      val dataContainer = DataContainer(
        index = index,
        currentDataLength = currentDataLength,
        data = DNACodec.charset
          .decode(
            ByteBuffer.wrap(data.slice(2, currentDataLength))
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
    output.write(elem.data.toArray)
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
