package se.scilifelab.polymerase

import scala.util.Random

object TestUtils {

  def createUnencodedDataPackage(
      data: List[Byte],
      blockLength: Int
  ): Seq[Package] = {
    val numberOfBlocks = (data.size.toDouble / blockLength).ceil.toInt
    data
      .grouped(blockLength)
      .zipWithIndex
      .map {
        case (data, index) =>
          Package.fromBytes(
            inputIndex = index,
            inputTotalNumberOfBlocks = numberOfBlocks,
            inputBlockLength = blockLength,
            inputData = data.toArray
          )
      }
      .toSeq
  }

  def corruptPackage(
      pck: Package,
      nbrOfErrors: Int,
      randomSeed: Int = 123
  ): Package = {
    val random = new Random(randomSeed)
    val bytes = pck.bytes
    val corrupted = (0 until nbrOfErrors).foldLeft(bytes) {
      case (bytes, _) =>
        val corruptIndex = random.nextInt(bytes.length)
        bytes.updated(
          corruptIndex,
          UnsignedByte((bytes(corruptIndex) + 1) % 256)
        )
    }
    Package.fromRawBytes(corrupted)
  }

  def corruptMessage(
      mess: Array[Int],
      nbrOfErrors: Int,
      randomSeed: Int = 123
  ): Array[Int] = {
    val random = new Random(randomSeed)
    if (mess.isEmpty) {
      mess
    } else {
      (0 until nbrOfErrors).foldLeft(mess) {
        case (mess, i) =>
          val corruptIndex = random.nextInt(mess.length)
          mess.updated(corruptIndex, (mess(corruptIndex) + 1) % 256)
      }
    }
  }
}
