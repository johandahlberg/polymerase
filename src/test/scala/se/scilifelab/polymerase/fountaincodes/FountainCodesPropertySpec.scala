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

  // This disables shrinking, which seems to cause more harm than
  // good here.
  import org.scalacheck.Shrink.shrinkAny

  // TODO Data packages need to be same length.
  // so zero padding is necessary on shorter ones.
  // Fix this on unencoded packages.
  val dataPackages =
    Gen.nonEmptyListOf(
      Gen.listOfN(5, Arbitrary.arbitrary[Byte].map(UnsignedByte(_)))
    )

//  def createUnencodedDataPackage(
//      data: List[List[UnsignedByte]],
//      blockLength: Int
//  ): List[UnencodedPackage] = {
//    data.grouped(blockLength).zipWithIndex.map {
//      case (data, index) =>
//        UnencodedPackage(
//          index = index,
//          length = data.length,
//          inputData = data.toArray,
//          blockLenght = blockLength
//        )
//    }
//  }
//
//  def dataToUnencodedPackages(
//      data: Seq[Seq[Int]],
//      blockLength: Int
//  ): Seq[UnencodedPackage] = {
//    data.zipWithIndex.map {
//      case (data, index) =>
//        UnencodedPackage(
//          index = index,
//          length = data.length,
//          inputData = data.map(UnsignedByte(_)).toArray,
//          blockLenght = blockLength
//        )
//    }
//  }
//
//  "The Fountain Code encoder/decoder" should "encode and decode any message" in {
//    forAll(dataPackages) { packages =>
//      {
//        val unencoded = createUnencodedDataPackage(packages, 5)
//        val codec = new FountainsCodes()
//        val encoded = codec.encode(unencoded)
//        val (res, nbrOfDecodedBlocks) =
//          codec.decode(encoded.toSeq, unencoded.length)
//
//        nbrOfDecodedBlocks should equal(unencoded.length)
//        unencoded.map(_.data.toList) should equal(res.map(_.data.toList).toList)
//      }
//    }
//  }
//
//  it should "encode/decode when messages when some packets are lost" in {
//    forAll(dataPackages) { packages =>
//      {
//        val unencoded = createUnencodedDataPackage(packages)
//        val codec = new FountainsCodes()
//        val encoded = codec.encode(unencoded)
//        val dropped = encoded.toSeq.drop(1)
//        val (res, nbrOfDecodedBlocks) =
//          codec.decode(dropped.toSeq, unencoded.length)
//
//        nbrOfDecodedBlocks should equal(unencoded.length)
//        unencoded.map(_.data.toList) should equal(res.map(_.data.toList).toList)
//      }
//    }
//  }
}
