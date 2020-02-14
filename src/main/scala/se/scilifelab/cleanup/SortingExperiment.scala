package se.scilifelab

import scala.collection.mutable.SortedSet
import scala.util.Random

// Assumptions we need:
//  - each element only needs to be output once
//  - element represents indexes, starting from 0 going to n
//  - elements need to be output consequtively, or not at all. I.e. 0,1,2,3,4,5

class ConsecutiveIterator(input: Iterator[Int]) extends Iterator[Int] {

  private var nextElementToOutput = 0
  private val queue = SortedSet[Int]()
  def hasNext() = input.hasNext || queue.nonEmpty

  def addUntilElementFound(): Boolean = {
    println("Trying to add more from input")
    val foundAndValue = input
      .filterNot(x => x < nextElementToOutput)
      .scanLeft((true, scala.collection.immutable.SortedSet[Int]())) {
        case ((goOn, soFar), elem) =>
          if (elem == nextElementToOutput) {
            (false, soFar + elem)
          } else {
            (true, soFar + elem)
          }
      }
      .find(!_._1)

    foundAndValue match {
      case None =>
        false
      case Some((_, valuesToAdd)) =>
        queue.addAll(valuesToAdd)
        println(s"valuesToAdd: $valuesToAdd")
        true
    }

  }

// TODO Figure out how to end interator at appropriate point. Now it tries to go on after last element.

  def next() = {
    println(
      s"queue is: $queue, nextElem is: $nextElementToOutput inputHasNext: ${input.hasNext} iteratorHasNext: ${hasNext}"
    )
    if (queue.isEmpty && input.hasNext) {
      addUntilElementFound()
      next()
    } else {
      val nextInQueue = queue.head
      if (nextInQueue == nextElementToOutput) {
        queue.remove(nextElementToOutput)
        nextElementToOutput += 1
        nextInQueue
      } else {
        addUntilElementFound()
        next()
      }
    }
  }
}

object SorterApp extends App {

  val input = Random.shuffle((0 to 10) ++ (0 to 10)) //.filterNot(x => x == 7)
  println(s"Input is: $input")
  val output = new ConsecutiveIterator(input.iterator)
  output.foreach(println)

}
