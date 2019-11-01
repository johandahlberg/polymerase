package se.scilifelab.reedsolomon

import scala.util.Random

object TestUtils {

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
