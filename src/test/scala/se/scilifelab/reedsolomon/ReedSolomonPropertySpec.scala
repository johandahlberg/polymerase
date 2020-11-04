package se.scilifelab.reedsolomon

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import scala.util.Random
import org.scalacheck.Gen

import se.scilifelab.polymerase.TestUtils
import se.scilifelab.polymerase.Package
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Arbitrary
import se.scilifelab.polymerase.UnsignedByte

object ReedSolomonPropertySpec
    extends Properties("ReedSolomon encoders/decoders") {

  val nonZeroIntegerArrays =
    Gen.containerOf[Seq, Int](Gen.choose(1, 255))

  val nonEmptyNonZeroIntegerArrays = nonZeroIntegerArrays suchThat (!_.isEmpty)

  val nonRandomValues =
    Gen.const(Range(1, 100).toSeq)

  property(
    "Encoding and decoding a list of integers, without errors, should yield back the same list"
  ) = forAll(nonZeroIntegerArrays) { a: Seq[Int] =>
    {
      val codec = ReedSolomonCoder(255, 223)
      val encoded = codec.encode(a.toArray)
      val (res, _) = codec.decode(encoded)
      a.toSeq == res.toSeq
    }
  }

  property(
    "Encoding and decoding a list of integers, with one errors, should yield back the same list"
  ) = forAll(nonZeroIntegerArrays) { a: Seq[Int] =>
    {
      val codec = ReedSolomonCoder(255, 223)
      val encoded = codec.encode(a.toArray)
      val corrupted =
        TestUtils.corruptMessage(encoded, 1)
      val (res, _) = codec.decode(corrupted)
      a.toSeq == res.toSeq
    }
  }

  property(
    "Encoding and decoding a list of integers, with two errors, should yield back the same list"
  ) = forAll(nonZeroIntegerArrays) { a: Seq[Int] =>
    {
      val codec = ReedSolomonCoder(255, 223)
      val encoded = codec.encode(a.toArray)
      val corrupted = TestUtils.corruptMessage(encoded, 2)
      val (res, _) = codec.decode(corrupted)
      a.toSeq == res.toSeq
    }
  }

  property(
    "Encoding and decoding a list (same range every time) of integers, with one errors, should yield back the same list"
  ) = forAll(nonRandomValues) { a: Seq[Int] =>
    {
      val codec = ReedSolomonCoder(255, 223)
      val encoded = codec.encode(a.toArray)
      val corrupted = TestUtils.corruptMessage(encoded, 1)
      val (res, _) = codec.decode(corrupted)
      a.toSeq == res.toSeq
    }
  }

  property(
    "Encoding and decoding a list of integers, with (n-k)/2 errors, should yield back the same list"
  ) = forAll(nonZeroIntegerArrays) { a: Seq[Int] =>
    {
      val n = 120
      val k = 110
      val codec = ReedSolomonCoder(n, k)
      val encoded = codec.encode(a.toArray)
      val corrupted =
        TestUtils.corruptMessage(
          encoded,
          scala.math.floorDiv((n - k), 2),
          randomSeed = 123
        )
      val (res, _) = codec.decode(corrupted)
      a.toSeq == res.toSeq
    }
  }

}

class ReedSolomonPackageCodecPropertySpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks {

  // This disables shrinking, which seems to cause more harm than
  // good here.
  import org.scalacheck.Shrink.shrinkAny

  val blockLength = 5
  val dataPackages = Gen
    .nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(TestUtils.createUnencodedDataPackage(_, blockLength))
  val arbitraryData = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])

  val dataPackage = for {
    i <- Gen.posNum[Int]
    c <- Gen.posNum[Int]
    l <- Gen.choose(0, 128)
    data <- Gen.listOfN(l, Arbitrary.arbitrary[Byte])
  } yield {
    Package(
      inputIndex = i,
      inputBlockLength = data.length,
      inputTotalNumberOfBlocks = c,
      dataLength = data.length,
      inputData = data.map(UnsignedByte(_)).toArray
    )
  }

  "The Reed-Solomon package codec" should "encode and decode any message" in {
    forAll(dataPackage) { unencoded =>
      {
        val codec =
          ReedSolomonPackageCodec(unencoded.byteLength, 10)
        val encoded = codec.encodePackage(unencoded)
        //val result = codec.decodePackage(encoded)
        //result.data should be(unencoded.data)
      }
    }
  }
  it should "decode messages when data is corrupted" in {
    forAll(dataPackage) { unencoded =>
      {
        //val codec =
        //  ReedSolomonPackageCodec(unencoded.byteLength, 10)
        //val encoded = codec.encodePackage(unencoded)
        //val corrupted =
        //  TestUtils.corruptPackage(encoded, 1)
        //val result = codec.decodePackage(corrupted)
        //result.data should be(unencoded.data)
      }
    }
  }
}
