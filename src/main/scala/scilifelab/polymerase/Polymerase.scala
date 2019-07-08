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
import scala.collection.mutable

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

object PolymeraseSplit extends App {

  val splitSize = 100

  val input = System.in
  val output = new DataOutputStream(new BufferedOutputStream(System.out))

  def inputToDataContainers(input: InputStream): Iterator[DataContainer] = {
    Source
      .fromInputStream(input)
      .grouped(splitSize)
      .zipWithIndex
      .map {
        case (data, index) => {
          DataContainer(index = index, data = data.toArray)
        }
      }
  }

  for { dataContainer <- inputToDataContainers(input) } {
    output.write(dataContainer.toByteArray())
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
      val dataContainer = DataContainer.fromInputStream(input)
      sortedInput(dataContainer) = true
    }
  } catch {
    case e: EOFException =>
  }

  for {
    elem <- sortedInput
  } {
    output.write(elem.data)
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
