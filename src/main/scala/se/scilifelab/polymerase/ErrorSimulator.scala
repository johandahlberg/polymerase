package se.scilifelab.polymerase

import scala.util.Random

object ErrorSimulator {
  private val probOfError = 0.006
  private val randomGenerator = new Random()
  private def addError: Boolean = randomGenerator.nextFloat < probOfError

  def addErrors(
      input: Iterator[Nucleotide]
  ): Iterator[Nucleotide] = {
    val res =
      for { nuc <- input.toSeq } yield {
        if (addError) {
          nucleotides.filter(p => p != nuc)(
            randomGenerator.nextInt(nucleotides.size - 1)
          )
        } else {
          nuc
        }
      }
    res.iterator
  }
}
