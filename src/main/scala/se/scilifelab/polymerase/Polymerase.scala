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
  val output = new BufferedOutputStream(System.out)
  val inputBytes =
    Stream.continually(input.read()).takeWhile(_ != -1).map(_.toByte).toIterator
  val encoded = ReedSolomonDNACodec.encode(inputBytes)
  encoded.foreach(x => output.write(x))

  input.close()
  output.flush()
  output.close()
}

object PolymeraseRSDecode extends App {

  val input = Source.fromInputStream(System.in)
  val output = new BufferedOutputStream(System.out)

  val data = ReedSolomonDNACodec.decode(input.iter)
  data.foreach(x => output.write(x))

  input.close()
  output.flush()
  output.close()

}

//object PolymeraseRSDecode extends App {
//val input = System.in
//val output = new DataOutputStream(new BufferedOutputStream(System.out))
//
//var blocks = 0
//for {
//byte <- ReedSolomonDNACodec.decode(Source.fromInputStream(input))
//} {
//output.write(byte)
//blocks += 1
//}
//
//input.close()
//output.flush()
//output.close()
//System.err.println(s"nbr of blocks in decode: $blocks")
//}

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
