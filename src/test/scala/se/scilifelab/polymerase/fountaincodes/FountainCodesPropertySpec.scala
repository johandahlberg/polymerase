package se.scilifelab.polymerase.fountaincodes

import scala.util.Random
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest._
import matchers.should.Matchers._
import org.scalacheck.Arbitrary
import se.scilifelab.polymerase.UnsignedByte
import se.scilifelab.polymerase.Package
import se.scilifelab.polymerase.TestUtils

class FountainCodesPropertySpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks {

  // This disables shrinking, which seems to cause more harm than
  // good here.
  import org.scalacheck.Shrink.shrinkAny

  val blockLength = 5
  val dataPackages = Gen
    .nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(TestUtils.createUnencodedDataPackage(_, blockLength))

  "The Fountain Code encoder/decoder" should "encode and decode any message" in {
    forAll(dataPackages) { unencoded =>
      {
        val codec = new FountainsCodes()
        val encoded = codec.encode(unencoded)
        val (res, nbrOfDecodedBlocks) =
          codec.decode(encoded)

        nbrOfDecodedBlocks should equal(unencoded.length)
        unencoded.map(_.data.toList) should equal(res.map(_.data.toList).toList)
      }
    }
  }

  it should "encode/decode when messages when some packets are lost" in {
    forAll(dataPackages) { unencoded =>
      {
        val codec = new FountainsCodes()
        val encoded = codec.encode(unencoded)
        val dropped = encoded.toSeq.drop(1)
        val (res, nbrOfDecodedBlocks) =
          codec.decode(dropped.iterator)

        nbrOfDecodedBlocks should equal(unencoded.length)
        unencoded.map(_.data.toList) should equal(res.map(_.data.toList).toList)
      }
    }
  }

}
