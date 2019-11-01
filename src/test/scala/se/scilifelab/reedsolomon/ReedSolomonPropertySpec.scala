package se.scilifelab.reedsolomon

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import scala.util.Random
import org.scalacheck.Gen

import se.scilifelab.reedsolomon.TestUtils
import org.scalatest.Matchers

object ReedSolomonPropertySpec
    extends Properties("ReedSolomon encoders/decoders")
    with Matchers {

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
      val corrupted = TestUtils.corruptMessage(encoded, 1)
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
