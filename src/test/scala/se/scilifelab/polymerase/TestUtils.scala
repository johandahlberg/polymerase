package se.scilifelab.polymerase

import scala.util.Random

object TestUtils {

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

}
