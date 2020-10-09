package se.scilifelab.polymerase

import java.io.DataInputStream
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.ByteArrayInputStream
import java.io.PrintWriter
import scala.io.Source

import se.scilifelab.polymerase._
import se.scilifelab.reedsolomon.{Defaults => RSDefaults}
import se.scilifelab.polymerase.fountaincodes.FountainsCodes
import java.nio.charset.StandardCharsets

trait EncoderApp extends App {

  def preEncoding(data: Iterator[Int]): Iterator[Array[Int]]
  def encode(data: Iterator[Array[Int]]): Iterator[Array[Nucleotide]]

  val input = new DataInputStream(new BufferedInputStream(System.in))
  val output = new PrintWriter(new BufferedOutputStream(System.out))

  val inputBytes =
    LazyList
      .continually(input.read())
      .takeWhile(_ != -1)
      .toIterator
  val encoded =
    encode(preEncoding(inputBytes))

  encoded.zipWithIndex.foreach {
    case (x, i) =>
      output.println(s">dna $i")
      output.println(x)
  }

  input.close()
  output.flush()
  output.close()
}

trait DecoderApp extends App {

  def preDecode(data: Iterator[String]): Iterator[Array[Nucleotide]]
  def decode(data: Iterator[Array[Nucleotide]]): Iterator[Byte]

  val input = Source.fromInputStream(System.in)
  val lines = input.getLines().filter(s => !s.startsWith(">"))
  val output = new BufferedOutputStream(System.out)

  val data = decode(preDecode(lines))
  data.foreach(x => output.write(x))

  input.close()
  output.flush()
  output.close()
}

object PolymeraseEncode extends EncoderApp {
  lazy val blockSize = 128
  def preEncoding(data: Iterator[Int]): Iterator[Array[Int]] = {
    data.grouped(blockSize).map(x => x.toArray)
  }
  def encode(data: Iterator[Array[Int]]): Iterator[Array[Nucleotide]] = {
    DNACodec.encodeBlocks(data)
  }
}

object PolymeraseDecode extends DecoderApp {
  def preDecode(data: Iterator[String]): Iterator[Array[Nucleotide]] = {
    data.map(_.toArray)
  }
  def decode(data: Iterator[Array[Nucleotide]]): Iterator[Byte] = {
    DNACodec.decodeBlocks(data)
  }
}

object PolymeraseRSEncode extends EncoderApp {

  def preEncoding(data: Iterator[Int]): Iterator[Array[Int]] = {
    data.grouped(ReedSolomonDNACodec.writeBlockSize).map(_.toArray)
  }
  def encode(data: Iterator[Array[Int]]): Iterator[Array[Nucleotide]] = {
    ReedSolomonDNACodec.encodeBlock(data)
  }
}

object PolymeraseRSDecode extends DecoderApp {

  def preDecode(data: Iterator[String]): Iterator[Array[Nucleotide]] = {
    data.map(s => s.toArray)
  }
  def decode(data: Iterator[Array[Nucleotide]]): Iterator[Byte] = {
    ReedSolomonDNACodec.decode(data)
  }
}

object PolymeraseFountainEncode extends App {

  val codec = new FountainsCodes(packageMultiplicationFactor = 3)

  val input = new DataInputStream(new BufferedInputStream(System.in))
  val output = new PrintWriter(new BufferedOutputStream(System.out))

  val iterator =
    LazyList.continually(input.read().toByte).takeWhile(_ != -1).iterator
  val pcksIterator = PackageEncoder.encode(iterator, 128)
  val pcks = pcksIterator.toSeq
  pcks.foreach(pck => System.err.println(s"ENCODE: $pcks"))

  val encodedPcks = codec.encode(pcks).toSeq
  encodedPcks.foreach(pck => System.err.println(s"ENCODE FOUNTAIN: $pcks"))

  val dnaEncoded = PackageDNACodec.encode(encodedPcks.iterator)

  dnaEncoded.zipWithIndex.foreach {
    case (x, i) =>
      output.println(s">dna $i")
      output.println(x)
  }

  input.close()
  output.flush()
  output.close()

}

object PolymeraseFountainDecode extends App {

  val input = Source.fromInputStream(System.in)
  val lines = input.getLines().filter(s => !s.startsWith(">"))
  val output = new BufferedOutputStream(System.out)

  val dnaDecodedPackages = lines
    .map(line => PackageDNACodec.decode(line.toArray))
    .map { pck =>
      Package.fromRawBytes(pck)
    }

  val fountainCodec = new FountainsCodes()
  val (decodedPackages, nbrOfPackagesDecoded) =
    fountainCodec.decode(dnaDecodedPackages, Int.MaxValue)

  decodedPackages
    .take(nbrOfPackagesDecoded)
    .foreach(pck => output.write(pck.data.map(_.underlyingByte)))

  input.close()
  output.flush()
  output.close()

}

object PolymeraseSimulateErrors extends App {
  val input = Source.fromInputStream(System.in)
  val lines = input.getLines()
  val output = new PrintWriter(new BufferedOutputStream(System.out))

  for { line <- lines } {
    if (line.startsWith(">")) {
      output.println(line)
    } else {
      output.println(ErrorSimulator.addErrors(line.iterator).mkString)
    }
  }

  input.close()
  output.flush()
  output.close()
}

object PolymeraseDropReads extends App {
  val input = Source.fromInputStream(System.in)
  val lines = input.getLines()
  val output = new PrintWriter(new BufferedOutputStream(System.out))

  for { line <- ErrorSimulator.dropRead(lines) } {
    output.println(line)
  }

  input.close()
  output.flush()
  output.close()
}
