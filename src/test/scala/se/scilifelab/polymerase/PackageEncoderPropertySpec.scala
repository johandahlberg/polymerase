package se.scilifelab.polymerase

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Arbitrary.arbitrary

import se.scilifelab.polymerase.ByteEncodable._
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

class PackageEncoderPropertySpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks {

  "The encode/decoder" should "encode/decode any single byte" in {
    forAll("byte") { byte: Byte =>
      PackageEncoder
        .decode(PackageEncoder.encode(Iterator.single(byte), 10))
        .next should equal(
        byte
      )
    }
  }

  it should "encode/decode any incoming stream of bytes" in {
    forAll("byte stream") { bytes: Seq[Byte] =>
      PackageEncoder
        .decode(PackageEncoder.encode(bytes.iterator, 10))
        .toSeq should equal(
        bytes
      )
    }
  }

  it should "encode/decode ints" in {
    forAll("int") { int: Int =>
      ByteEncodable.decode[Int](
        PackageEncoder
          .decode(PackageEncoder.encode(ByteEncodable.encode(int), 10))
      ) should equal(int)
    }
  }

  it should "encode/decode strings" in {
    forAll("string") { string: String =>
      ByteEncodable.decode[String](
        PackageEncoder
          .decode(PackageEncoder.encode(ByteEncodable.encode(string), 10))
      ) should equal(string)
    }
  }

  it should "encode/decode serializable" in {

    val testClass = TestSerializable("string")

    val byteStream = new ByteArrayOutputStream()
    val objectStream = new ObjectOutputStream(byteStream)

    objectStream.writeObject(testClass)
    objectStream.flush()
    val bytes = byteStream.toByteArray()

    objectStream.close()
    byteStream.close()
    val encoded = PackageEncoder.encode(bytes.iterator, 10)
    val decoded = PackageEncoder.decode(encoded)

    val inputByteStream = new ByteArrayInputStream(decoded.toArray)
    val inputStream = new ObjectInputStream(inputByteStream)
    val testClassDecoded =
      inputStream.readObject().asInstanceOf[TestSerializable]

    testClass should equal(testClassDecoded)
  }

}

// This has to be placed outside the test class, because
// otherwise the serializer trys to serialize the entire
// context, which will not work.
case class TestSerializable(x: String)
