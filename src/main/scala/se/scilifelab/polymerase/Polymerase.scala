package se.scilifelab.polymerase

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
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

import util.control.Breaks._
import scala.util.Random

object PolymeraseEncode extends App {

  val psudoRandomGenerator = new Random(9876L)

  val input = new DataInputStream(new BufferedInputStream(System.in))
  val output = new PrintWriter(new BufferedOutputStream(System.out))
  try {
    while (true) {
      val data = input.readByte()
      val randomizedData = (data ^ psudoRandomGenerator.nextInt()).toByte
      output.write(DNACodec.encode(randomizedData).toArray)
    }
  } catch {
    case e: EOFException =>
  } finally {
    input.close()
    output.flush()
    output.close()
  }
}

object PolymeraseDecode extends App {
  val input = System.in
  val output = new DataOutputStream(new BufferedOutputStream(System.out))

  val psudoRandomGenerator = new Random(9876L)

  for {
    randomByte <- DNACodec.decode(Source.fromInputStream(input))
  } {
    val byte = (randomByte ^ psudoRandomGenerator.nextInt()).toByte
    output.write(byte)
  }

  input.close()
  output.flush()
  output.close()
}

object PolymeraseSplit extends App {

  val splitSize = 100

  val input = System.in
  val output = new ObjectOutputStream(new BufferedOutputStream(System.out))

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

  var c = 0
  for { dataContainer <- inputToDataContainers(input) } {
    output.writeObject(dataContainer)
    c += 1
  }
  output.flush()

  System.err.println(s"Wrote $c data containers")

}

object PolymeraseJoin extends App {
  val input = new ObjectInputStream(new BufferedInputStream(System.in))
  val output = new PrintWriter(new BufferedOutputStream(System.out))

  // Todo this should probably be backed by disk spilling collection later
  // so as to not run out of all memory. /JD 2019-07-08
  val sortedInput = scala.collection.mutable.SortedSet[DataContainer]()

  var c = 0
  try {
    while (true) {
      val obj = input.readObject()
      c += 1
      val dataContainer = obj.asInstanceOf[DataContainer]
      sortedInput(dataContainer) = true
    }
  } catch {
    case e: EOFException => {
      System.err.println(s"Found end of file after reading $c data containers")
    }
  }

  for {
    elem <- sortedInput
  } {
    output.write(elem.data)
  }
  input.close()
  output.flush()
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
