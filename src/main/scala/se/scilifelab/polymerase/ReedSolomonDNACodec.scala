package se.scilifelab.polymerase

import se.scilifelab.reedsolomon.ReedSolomonCoder
import se.scilifelab.reedsolomon.{Defaults => RSDefaults}
import java.nio.ByteBuffer
import scala.util.Random

object ReedSolomonDNACodec {

  // TODO These settings generate to long DNA sequences
  // per block. Look at better values.
  private val rsCoder =
    ReedSolomonCoder(n = RSDefaults.dictonarySize, k = RSDefaults.messageSize)

  // TODO Psudo randomize bytes, to avoid monomer stretches.
  //      Or better still, rotate bases as described in X.

  def encode(data: Iterator[Byte]): Iterator[Nucleotide] = {

    val groupedDataWithLength = data
    // Use the integers to encode length and index of message
      .grouped(RSDefaults.messageSize - Integer.BYTES * 2)
      .zipWithIndex
      .map {
        case (x, i) =>
          val index =
            ByteBuffer.allocate(4).putInt(i).array()
          val dataLength = x.length + index.length
          val dataAsBytes = (index ++ x).map(y => (y & (0xff)))
          (dataLength +: dataAsBytes).toArray
      }

    val rsEncodedData = groupedDataWithLength.map { mess =>
      rsCoder.encode(mess)
    }

    val tmpTestRandomOrder = Random.shuffle(rsEncodedData.toSeq).iterator

    val dnaEncodedData =
      DNACodec.encodeBlocks(tmpTestRandomOrder)

    dnaEncodedData

  }

  def iterateInIndexOrder(in: Iterator[(Int, Array[Byte])]): Iterator[Byte] = {

    val queue =
      scala.collection.mutable.PriorityQueue.empty[(Int, Array[Byte])](
        Ordering.by((_: (Int, Array[Byte]))._1 * -1)
      )

    def enqueUntilFound(index: Int): Unit = {

      import util.control.Breaks._

      breakable {
        while (in.hasNext) {
          val next = in.next()
          if (next._1 == index) {
            System.err.println("Found index")
            queue.enqueue(next)
            break()
          } else {
            System.err.println("Found out of order index")
            queue.enqueue(next)
          }
        }
      }
    }

    def iterateIndexes(indexes: Iterator[Int]): Iterator[Byte] = {
      val indexToSearchFor = indexes.next()
      System.err.println(s"indexToSearchFor: $indexToSearchFor")
      if (queue.find(_._1 == indexToSearchFor).isDefined) {
        System.err.println(s"Found index in queue")
        Iterator.single(queue.dequeue()._2).flatten
      } else {
        System.err.println(s"Did not find index in queue")
        enqueUntilFound(indexToSearchFor)
        Iterator.single(queue.dequeue()._2).flatten
      }
    }

    // TODO Not sure if the -1 above should be there or not...
    // Depends on how ordering is working...
    val indexStream = Iterator.from(0)

    val found = indexStream
      .takeWhile(_ => in.hasNext)
      .map { indexToSearchFor =>
        System.err.println(s"indexToSearchFor: $indexToSearchFor")
        queue.enqueue(in.next())
        val indexInQueue = queue.find(_._1 == indexToSearchFor)
        if (indexInQueue.isDefined) {
          System.err.println(s"Found index in queue")
          Iterator.single(indexInQueue.get._2).flatten
        } else {
          System.err.println(s"Did not find index in queue")
          enqueUntilFound(indexToSearchFor)
          Iterator.single(queue.dequeue()._2).flatten
        }
      }
      .flatten

    val elems = queue.dequeueAll
    val rest = elems.map(_._2).iterator.flatten

    // .iterator
    //   .map((x: Iterator[(Int, Array[Byte])]) => x._2)
    //   .flatten

    //val rest = Iterator
    //  .continually(queue.dequeue()._2.iterator)
    //  .takeWhile(_ => !queue.isEmpty)
    //  .flatten

    found ++ rest
  }

  def decode(data: Iterator[Nucleotide]): Iterator[Byte] = {

    val bytes = DNACodec.decode(data)

    val byteBlocks =
      bytes
        .map(x => x & (0xff))
        .grouped(RSDefaults.dictonarySize)

    val indexesAndData = byteBlocks
      .map { x =>
        rsCoder.decode(x.toArray)._1
      }
      .map { res =>
        // Only pick up the original data, i.e. strip the length of the data block,
        // and the index.
        val length = res.head
        val index = ByteBuffer.wrap(res.tail.take(4).map(_.toByte)).getInt()
        val data = res.drop(5).take(length).map(_.toByte)
        (index, data)
      }
    //iterateInIndexOrder(indexesAndData)
    indexesAndData.toSeq.sortBy(_._1).map(_._2).flatten.iterator
  }

}
