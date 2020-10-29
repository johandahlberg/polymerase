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

}
