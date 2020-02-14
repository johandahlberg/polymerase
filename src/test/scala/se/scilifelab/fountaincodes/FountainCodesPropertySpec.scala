package se.scilifelab.fountaincodes

import scala.util.Random
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest._
import matchers.should.Matchers._

class FountainCodesPropertySpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks {

  val nbrOfBlocks = 100
  val sizeOfBlocks = 1000
  val blocks =
    Gen
      .listOfN(
        nbrOfBlocks,
        Gen.listOfN(sizeOfBlocks, Gen.choose(1, 255).map(_.toByte))
      )
      .suchThat(x => !x.exists(y => y.length < sizeOfBlocks))

  "The Fountain Code encoder/decoder" should "encode and decode any message" in {
    forAll(blocks) { blocksToTry: Seq[Seq[Byte]] =>
      {
        val codec = new FountainsCodes()
        val encoded = codec.encode(blocksToTry)
        val (res, nbrOfDecodedBlocks) =
          codec.decode(encoded.toSeq, blocksToTry.length)
        nbrOfDecodedBlocks should equal(blocksToTry.length)
        blocksToTry.toSeq.map(_.toSeq) should equal(res.map(_.toSeq))
      }
    }
  }

  it should "encode/decode when messages when some packets are lost" in {
    forAll(blocks) { blocksToTry: Seq[Seq[Byte]] =>
      {
        val codec = new FountainsCodes()
        val encoded = codec.encode(blocksToTry)
        val encodedDroppedBlocks = encoded.drop(5)
        val (res, nbrOfDecodedBlocks) =
          codec.decode(encodedDroppedBlocks.toSeq, blocksToTry.length)
        nbrOfDecodedBlocks should equal(blocksToTry.length)
        blocksToTry.toSeq.map(_.toSeq) should equal(res.map(_.toSeq))
      }
    }
  }
}
