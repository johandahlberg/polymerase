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

import java.io.InputStream
import java.nio.CharBuffer
import scala.collection.mutable
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

import util.control.Breaks._
import scala.util.Random

import se.scilifelab.polymerase._

import se.scilifelab.reedsolomon.{Defaults => RSDefaults}

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

object PolymeraseRSEncode extends App {

  val input = new DataInputStream(new BufferedInputStream(System.in))
  val output = new PrintWriter(new BufferedOutputStream(System.out))
  val inputBytes =
    LazyList
      .continually(input.read())
      .takeWhile(_ != -1)
      .map(_.toByte)
      .toIterator
  val encoded = ReedSolomonDNACodec.encode(inputBytes)
  encoded.zipWithIndex.foreach {
    case (x, i) =>
      output.println(s">dna $i")
      output.println(x)
  }

  input.close()
  output.flush()
  output.close()
}

object PolymeraseRSDecode extends App {

  val input = Source.fromInputStream(System.in)
  val lines = input.getLines().filter(s => !s.startsWith(">"))
  val output = new BufferedOutputStream(System.out)

  val data = ReedSolomonDNACodec.decode(lines.map(x => x.toArray))
  data.foreach(x => output.write(x))

  input.close()
  output.flush()
  output.close()

}

object PolymeraseSimulateErrors extends App {
  val input = Source.fromInputStream(System.in)
  val output = new BufferedOutputStream(System.out)

  val data = ErrorSimulator.addErrors(input.iter)
  data.foreach(x => output.write(x))

  input.close()
  output.flush()
  output.close()
}
