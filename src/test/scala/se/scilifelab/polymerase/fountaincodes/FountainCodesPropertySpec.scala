package se.scilifelab.polymerase.fountaincodes

import scala.util.Random
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest._
import matchers.should.Matchers._
import se.scilifelab.polymerase.UnencodedPackage
import org.scalacheck.Arbitrary
import se.scilifelab.polymerase.UnsignedByte

class FountainCodesPropertySpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks {

  // TODO Data packages need to be same length.
  // so zero padding is necessary on shorter ones.
  // Fix this on unencoded packages.
  val dataPackages =
    Gen.nonEmptyListOf(
      Gen.listOfN(100, Arbitrary.arbitrary[Byte].map(UnsignedByte(_)))
    )

  def createUnencodedDataPackage(
      data: List[List[UnsignedByte]]
  ): List[UnencodedPackage] = {
    data.zipWithIndex.map {
      case (data, index) =>
        UnencodedPackage(index, data.length, data.toArray)
    }
  }

  "The Fountain Code encoder/decoder" should "encode and decode any message" in {
    forAll(dataPackages) { packages =>
      {
        val unencoded = createUnencodedDataPackage(packages)
        println(s"TEST: $unencoded")
        val codec = new FountainsCodes()
        val encoded = codec.encode(unencoded)
        val (res, nbrOfDecodedBlocks) =
          codec.decode(encoded.toSeq, unencoded.length)

        nbrOfDecodedBlocks should equal(unencoded.length)
        unencoded.map(_.data) should equal(res.map(_.data))
      }
    }
  }

  //it should "encode/decode when messages when some packets are lost" in {
  //  forAll(blocks) { blocksToTry: Seq[Seq[Byte]] =>
  //    {
  //      val codec = new FountainsCodes()
  //      val encoded = codec.encode(blocksToTry)
  //      val encodedDroppedBlocks = encoded.drop(5)
  //      val (res, nbrOfDecodedBlocks) =
  //        codec.decode(encodedDroppedBlocks.toSeq, blocksToTry.length)
  //      nbrOfDecodedBlocks should equal(blocksToTry.length)
  //      blocksToTry.toSeq.map(_.toSeq) should equal(res.map(_.toSeq))
  //    }
  //  }
  //}
}
