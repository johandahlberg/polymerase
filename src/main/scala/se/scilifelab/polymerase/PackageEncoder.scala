package se.scilifelab.polymerase

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer

case class UnencodedPackage(
    index: Int,
    length: Int,
    blockLength: Int,
    inputData: Array[UnsignedByte]
) extends Package {}

trait ByteEncodable[A] {
  def encode(input: A): Iterator[Byte]
  def decode(input: Iterator[Byte]): A
}

object ByteEncodable {
  def encode[A: ByteEncodable](input: A): Iterator[Byte] =
    implicitly[ByteEncodable[A]].encode(input)
  def decode[A: ByteEncodable](input: Iterator[Byte]): A =
    implicitly[ByteEncodable[A]].decode(input)

  implicit val intEncodable = new ByteEncodable[Int] {
    def encode(input: Int): Iterator[Byte] =
      ByteBuffer.allocate(4).putInt(input).array().iterator
    def decode(input: Iterator[Byte]): Int =
      ByteBuffer.wrap(input.take(4).toArray).getInt
  }

  implicit val stringEncodable = new ByteEncodable[String] {
    def encode(input: String): Iterator[Byte] = {
      input.getBytes(StandardCharsets.UTF_8).iterator
    }
    def decode(input: Iterator[Byte]): String = {
      new String(input.toArray, StandardCharsets.UTF_8)
    }
  }
}

object PackageEncoder {

  def encode(
      input: Iterator[Byte],
      packageSize: Int
  ): Iterator[UnencodedPackage] = {
    input.grouped(packageSize).zipWithIndex.map {
      case (data, index) =>
        UnencodedPackage(
          index = index,
          length = data.length,
          inputData = data.map(UnsignedByte(_)).toArray,
          blockLength = packageSize
        )
    }
  }

  def decode(input: Iterator[UnencodedPackage]): Iterator[Byte] = {
    // TODO This materializes the full dataset here.
    // Would be better to sort it eventually.
    input.toSeq
      .sortBy(pack => pack.index)
      .flatMap(pack => pack.dataToDecode.map(_.underlyingByte))
      .iterator
  }

}
