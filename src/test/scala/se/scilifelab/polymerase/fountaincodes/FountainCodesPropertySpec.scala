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

class FountainCodesPropertySpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks {

  // This disables shrinking, which seems to cause more harm than
  // good here.
  import org.scalacheck.Shrink.shrinkAny

  val dataPackages = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])

  def createUnencodedDataPackage(
      data: List[Byte],
      blockLength: Int
  ): Seq[Package] = {
    data
      .grouped(blockLength)
      .zipWithIndex
      .map {
        case (data, index) =>
          Package.fromBytes(
            inputIndex = index,
            inputBlockLength = blockLength,
            inputData = data.toArray
          )
      }
      .toSeq
  }

  "The Fountain Code encoder/decoder" should "encode and decode any message" in {
    forAll(dataPackages) { packages =>
      {
        val unencoded = createUnencodedDataPackage(packages, 5)
        val codec = new FountainsCodes()
        val encoded = codec.encode(unencoded, 5)
        val (res, nbrOfDecodedBlocks) =
          codec.decode(encoded.toSeq, unencoded.length)

        nbrOfDecodedBlocks should equal(unencoded.length)
        unencoded.map(_.data.toList) should equal(res.map(_.data.toList).toList)
      }
    }
  }

  it should "encode/decode when messages when some packets are lost" in {
    forAll(dataPackages) { packages =>
      {
        val unencoded = createUnencodedDataPackage(packages, 5)
        val codec = new FountainsCodes()
        val encoded = codec.encode(unencoded, 5)
        val dropped = encoded.toSeq.drop(1)
        val (res, nbrOfDecodedBlocks) =
          codec.decode(dropped.toSeq, unencoded.length)

        nbrOfDecodedBlocks should equal(unencoded.length)
        unencoded.map(_.data.toList) should equal(res.map(_.data.toList).toList)
      }
    }
  }
}
