package se.scilifelab.polymerase

import scala.util.Random
import scala.collection.Iterator

object ErrorSimulator {
  private val randomGenerator = new Random()

  private def addError(probOfError: Double): Boolean =
    randomGenerator.nextFloat < probOfError

  private def dropRead(probOfPackageDrop: Double): Boolean =
    randomGenerator.nextFloat < probOfPackageDrop

  def addErrors(
      input: Iterator[Nucleotide],
      probOfError: Double = 0.001
  ): Iterator[Nucleotide] = {
    val res =
      for { nuc <- input.toSeq } yield {
        if (addError(probOfError)) {
          nucleotides.filter(p => p != nuc)(
            randomGenerator.nextInt(nucleotides.size - 1)
          )
        } else {
          nuc
        }
      }
    res.iterator
  }

  def dropRead(
      reads: Iterator[String],
      probOfPackageDrop: Double = 0.01
  ): Iterator[String] = {
    reads
      .grouped(2)
      .filterNot(_ => dropRead(probOfPackageDrop))
      .flatten
  }
}
