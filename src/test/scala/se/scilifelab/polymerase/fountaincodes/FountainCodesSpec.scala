package se.scilifelab.polymerase.fountaincodes

import org.scalatest._
import scala.annotation.meta.field
import se.scilifelab.reedsolomon.TestUtils

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import matchers.should.Matchers._
import se.scilifelab.polymerase.UnencodedPackage
import se.scilifelab.polymerase.UnsignedByte

class FountainCodeSpec extends AnyFlatSpec {

  def dataToUnencodedPackages(data: Seq[Seq[Int]]): Seq[UnencodedPackage] = {
    data.zipWithIndex.map {
      case (data, index) =>
        UnencodedPackage(
          index,
          length = data.length,
          data = data.map(UnsignedByte(_)).toArray
        )
    }
  }

  "The Fountain Code encoder" should "encode and decode input data (1)" in {

    val inputData = dataToUnencodedPackages(
      Seq(Seq(1, 2, 3, 4, 5), Seq(6, 7, 8, 9, 10), Seq(11, 12, 13, 14, 15))
    )
    val codec = new FountainsCodes()
    val encoded = codec.encode(inputData)
    val (decoded, nbrOfBlocksSolved) =
      codec.decode(encoded.toSeq, inputData.length)

    nbrOfBlocksSolved should be(inputData.length)
    decoded.map(_.data.toSeq) should be(inputData.map(_.data.toSeq))

  }

  //it should "encode and decode input data (2)" in {

  //  val inputData: Seq[Seq[Byte]] =
  //    Seq(
  //      Seq(1, 2, 3, 4, 5),
  //      Seq(6, 7, 8, 9, 10)
  //    )
  //  val codec = new FountainsCodes()
  //  val encoded = codec.encode(inputData)
  //  val (decoded, nbrOfBlocksSolved) =
  //    codec.decode(encoded.toSeq, inputData.length)

  //  nbrOfBlocksSolved should be(inputData.length)
  //  decoded should be(inputData)
  //}

  //it should "encode and decode input data (3)" in {

  //  val inputData: Seq[Seq[Byte]] =
  //    Seq(
  //      Seq(1, 2, 3, 4, 5),
  //      Seq(6, 7, 8, 9, 10),
  //      Seq(11, 12, 13, 14, 15),
  //      Seq(16, 17, 18, 19, 20)
  //    )
  //  val codec = new FountainsCodes()
  //  val encoded = codec.encode(inputData)
  //  val (decoded, nbrOfBlocksSolved) =
  //    codec.decode(encoded.toSeq, inputData.length)

  //  nbrOfBlocksSolved should be(inputData.length)
  //  decoded should be(inputData)
  //}

  //it should "encode and decode input data (4)" in {

  //  val inputData: Seq[Seq[Byte]] =
  //    Seq(
  //      Seq(1, 2, 3, 4, 5)
  //    )
  //  val codec = new FountainsCodes()
  //  val encoded = codec.encode(inputData)
  //  val (decoded, nbrOfBlocksSolved) =
  //    codec.decode(encoded.toSeq, inputData.length)

  //  nbrOfBlocksSolved should be(inputData.length)
  //  decoded should be(inputData)
  //}

  //it should "encode and decode input data (5)" in {

  //  val inputData: Seq[Seq[Byte]] =
  //    Seq.fill(5)(Seq.fill(5)(0))
  //  val codec = new FountainsCodes()
  //  val encoded = codec.encode(inputData)
  //  val (decoded, nbrOfBlocksSolved) =
  //    codec.decode(encoded.toSeq, inputData.length)

  //  nbrOfBlocksSolved should be(inputData.length)
  //  decoded should be(inputData)
  //}
}
