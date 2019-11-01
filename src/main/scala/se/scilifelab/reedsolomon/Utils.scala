package se.scilifelab.reedsolomon

import scala.math.sqrt
import scala.collection.BitSet

object Utils {

  // Picked up and adopted from: https://stackoverflow.com/a/26304447/1412088
  def primesUpTo(n: Int): BitSet = {
    val isPrime = collection.mutable.BitSet(2 to n: _*) -- (4 to n by 2)
    for (p <- 2 +: (3 to sqrt(n).toInt by 2) if isPrime(p)) {
      isPrime --= p * p to n by p
    }
    isPrime.toImmutable
  }

}
