package se.scilifelab.polymerase

import scala.util.Random

object ErrorSimulator {
  private val probOfError = 0.01
  private val randomGenerator = new Random()
  private def addError: Boolean = randomGenerator.nextFloat < probOfError

  def addErrors(
      input: Iterator[Nucleotide]
  ): Iterator[Nucleotide] = {
    for { nuc <- input } yield {
      if (addError) {
        nucleotides.filter(p => p != nuc)(
          randomGenerator.nextInt(nucleotides.size)
        )
      } else {
        nuc
      }
    }
  }
}
