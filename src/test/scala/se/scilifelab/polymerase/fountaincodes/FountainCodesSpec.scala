package se.scilifelab.polymerase.fountaincodes

import org.scalatest._
import scala.annotation.meta.field
import se.scilifelab.reedsolomon.TestUtils

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import matchers.should.Matchers._
import se.scilifelab.polymerase.UnsignedByte
import se.scilifelab.polymerase.Package

class FountainCodeSpec extends AnyFlatSpec {

  def dataToUnencodedPackages(
      data: Seq[Seq[Int]],
      blockLength: Int
  ): Seq[Package] = {
    data.zipWithIndex.map {
      case (data, index) =>
        Package(
          inputIndex = index,
          inputBlockLength = blockLength,
          dataLength = data.length,
          inputData = data.map(UnsignedByte(_)).toArray
        )
    }
  }

  "The Fountain Code encoder" should "encode and decode input data (1)" in {

    val inputData = dataToUnencodedPackages(
      Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15)
      ),
      blockLength = 5
    )
    val codec = new FountainsCodes()
    val encoded = codec.encode(inputData)
    val (decoded, nbrOfBlocksSolved) =
      codec.decode(encoded, inputData.length)

    nbrOfBlocksSolved should be(inputData.length)
    decoded.map(_.data.toSeq) should be(inputData.map(_.data.toSeq))

  }

  it should "encode and decode input data (2)" in {

    val inputData = dataToUnencodedPackages(
      Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10)
      ),
      blockLength = 5
    )
    val codec = new FountainsCodes()
    val encoded = codec.encode(inputData)
    val (decoded, nbrOfBlocksSolved) =
      codec.decode(encoded, inputData.length)

    nbrOfBlocksSolved should be(inputData.length)
    decoded.map(_.data.toSeq) should be(inputData.map(_.data.toSeq))
  }

  it should "encode and decode input data (3)" in {

    val inputData = dataToUnencodedPackages(
      Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20)
      ),
      blockLength = 5
    )
    val codec = new FountainsCodes()
    val encoded = codec.encode(inputData)
    val (decoded, nbrOfBlocksSolved) =
      codec.decode(encoded, inputData.length)

    nbrOfBlocksSolved should be(inputData.length)
    decoded.map(_.data.toSeq) should be(inputData.map(_.data.toSeq))
  }

  it should "encode and decode input data (4)" in {

    val inputData = dataToUnencodedPackages(
      Seq(
        Seq(1, 2, 3, 4, 5)
      ),
      blockLength = 5
    )
    val codec = new FountainsCodes()
    val encoded = codec.encode(inputData)
    val (decoded, nbrOfBlocksSolved) =
      codec.decode(encoded, inputData.length)

    nbrOfBlocksSolved should be(inputData.length)
    decoded.map(_.data.toSeq) should be(inputData.map(_.data.toSeq))
  }

  it should "encode and decode input data (5)" in {

    val inputData = dataToUnencodedPackages(
      Seq.fill(5)(Seq.fill(5)(0)),
      blockLength = 5
    )
    val codec = new FountainsCodes()
    val encoded = codec.encode(inputData)
    val (decoded, nbrOfBlocksSolved) =
      codec.decode(encoded, inputData.length)

    nbrOfBlocksSolved should be(inputData.length)
    decoded.map(_.data.toSeq) should be(inputData.map(_.data.toSeq))
  }

  it should "encode and decode input data (6)" in {

    val inputData = dataToUnencodedPackages(
      Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19)
      ),
      blockLength = 5
    )
    val codec = new FountainsCodes()
    val encoded = codec.encode(inputData)
    val (decoded, nbrOfBlocksSolved) =
      codec.decode(encoded, inputData.length)

    nbrOfBlocksSolved should be(inputData.length)
    decoded.map(_.data.toSeq) should be(inputData.map(_.data.toSeq))
  }

  it should "encode and decode input data when data dropped (1)" in {

    val inputData = dataToUnencodedPackages(
      Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20)
      ),
      blockLength = 5
    )
    val codec = new FountainsCodes()
    val encoded = codec.encode(inputData)
    val dropped = encoded.toSeq.drop(1)
    val (decoded, nbrOfBlocksSolved) =
      codec.decode(dropped.iterator, inputData.length)

    nbrOfBlocksSolved should be(inputData.length)
    decoded.map(_.data.toSeq) should be(inputData.map(_.data.toSeq))
  }

  it should "encode and decode input data when data dropped (2)" in {

    val inputData = dataToUnencodedPackages(
      Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19)
      ),
      blockLength = 5
    )
    val codec = new FountainsCodes()
    val encoded = codec.encode(inputData)
    val dropped = encoded.toSeq.drop(1)
    val (decoded, nbrOfBlocksSolved) =
      codec.decode(dropped.iterator, inputData.length)

    nbrOfBlocksSolved should be(inputData.length)
    decoded.map(_.data.toSeq) should be(inputData.map(_.data.toSeq))
  }

  it should "encode/devode specific example (7)" in {
    val inputData = dataToUnencodedPackages(
      Seq(60, 49, 0, 1, 1, -128, 0, 99, 125, -1, -128, -76, 108, -112, 127, -1,
        -111, 102, -128, -28, -46, -128, 91).grouped(5).toSeq,
      5
    )
    val codec = new FountainsCodes()
    val encoded = codec.encode(inputData)
    val dropped = encoded.toSeq.drop(1)
    val (decoded, nbrOfBlocksSolved) =
      codec.decode(dropped.iterator, inputData.length)

    nbrOfBlocksSolved should be(inputData.length)
    decoded.map(_.data.toSeq) should be(inputData.map(_.data.toSeq))
  }
}
