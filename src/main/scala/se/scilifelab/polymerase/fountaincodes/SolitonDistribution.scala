package se.scilifelab.polymerase.fountaincodes

import scala.util.Random
import scala.math.{log, sqrt}

class IdealSolitonGenerator(k: Int, seed: Option[Int] = None) {

  val random = seed match {
    case Some(i) => new Random(i)
    case None    => new Random()
  }

  def next(): Int = {
    val x = random.nextDouble()
    val i = (1 / x).ceil.toInt
    if (i > k) {
      1
    } else {
      i
    }
  }

}

class RobustSoliton(
    k: Int,
    sigma: Double,
    c: Double,
    seed: Option[Int] = None
) {

  private val random = seed match {
    case Some(i) => new Random(i)
    case None    => new Random()
  }

  private val R: Double = c * log(k.toDouble / sigma) * sqrt(k)
  private val beta = (1 to k).map(i => rho(i) + theta(i)).sum

  private def rho(i: Int): Double = idealSoliton(i)
  private def theta(i: Int): Double = {
    // The distribution is only defined for discrete values
    // Not 100% if this is the correct way to handle this.
    val m = (k / R).floor.toInt
    i match {
      case x if x > 0 && x <= m - 1 =>
        R / (k * i)
      case x if x == m =>
        (R * log(R / sigma)) / k
      case x if x >= m + 1 && x <= k =>
        0
    }
  }

  private def idealSoliton(i: Int): Double = {
    i match {
      case x if x == 1 =>
        1.0 / k
      case x if x >= 2 && x <= k =>
        1.0 / (x * (x - 1))
    }
  }

  private def robustSoliton(i: Int): Double = {
    (rho(i) + theta(i)) / beta
  }

  // TODO Think of if we really want to cache this or not.
  private val probDistribution =
    (1 to k).toStream.map(index => robustSoliton(index))

  // TODO This does not really work here, I think, because the distribution defined
  // above is not the cumulative distribution function, but the distribution function
  // However, this StackOverflow question appears to indidate that this is correct.
  // https://stackoverflow.com/questions/24869304/scala-how-can-i-generate-numbers-according-to-an-expected-distribution
  private def sampleFromDist(random: Double): Int = {
    val discreteValue =
      probDistribution
        .scanLeft(0.0) {
          case (sum, x) => sum + x
        }
        .takeWhile(_ <= random)
        .zipWithIndex
        .last
        ._2
    // Add one, because the distribution starts from k=1, not k=2
    discreteValue + 1
  }

  def next(): Int = {
    sampleFromDist(random.nextDouble())
  }

  def sample(n: Int): LazyList[Int] = {
    LazyList.from(0 to n).map(_ => next())
  }
}

object SolitonPlayground extends App {

  def printHistogram(numbers: Seq[Int]): Unit = {
    val counts = numbers.groupBy(x => x).mapValues(_.size)
    val total = counts.values.sum
    val sortedCounts = counts.toSeq.sortBy(_._1)
    sortedCounts.foreach {
      case (x, counts) =>
        val percent = (counts.toDouble / total) * 100
        println(f"$x\t$percent%1.2f\t${"x" * percent.toInt}")
    }
  }

  val randomSeed = 1234
  val nbrOfBlocks = 10000
  val trails = 10000
  val constant = 0.2
  val failureProb = 0.05

  //println("-----------------------------------------")
  //println("Ideal Soliton Generator")
  //println("-----------------------------------------")
  //val idealSolitonGenerator =
  //  new IdealSolitonGenerator(nbrOfBlocks, seed = Some(randomSeed))
  //val numbers = for {
  //  i <- 0 to trails
  //} yield {
  //  idealSolitonGenerator.next()
  //}
  //printHistogram(numbers)

  println("-----------------------------------------")
  println("Robust Soliton Generator")
  println("-----------------------------------------")
  val robustSolitonGenerator =
    new RobustSoliton(
      nbrOfBlocks,
      failureProb,
      constant,
      seed = Some(randomSeed)
    )
  val robustNumbers = for {
    i <- 0 to trails
  } yield {
    robustSolitonGenerator.next()
  }
  printHistogram(robustNumbers)
}
