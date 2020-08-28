package se.scilifelab.polymerase

import java.nio.ByteBuffer
import scala.collection.immutable.SortedMap

object CodecUtils {

  def encodeIntAsBytes(int: Int): Array[Byte] = {
    ByteBuffer.allocate(4).putInt(int).array()
  }

  def decodeIntFromBytes(bytes: Array[Byte]): Int = {
    ByteBuffer.wrap(bytes).getInt()
  }

  def createDataBlock(data: Iterator[Array[Int]]): Iterator[Array[Int]] = {
    data.zipWithIndex
      .map {
        case (x, i) =>
          val index =
            encodeIntAsBytes(i).map(_.toInt)
          val dataLength = x.length + index.length
          val dataAsBytes = (index ++ x).map(_ & 0xff)
          (dataLength +: dataAsBytes).toArray
      }
  }

  def deconstructDataBlock(data: Array[Int]): (Int, Array[Byte]) = {
    val length = data.head
    val index = decodeIntFromBytes(data.tail.take(4).map(_.toByte))
    val pck = data.drop(5).take(length).map(_.toByte)
    (index, pck)
  }

  def sortIndexAndDataOutput(
      indexesAndData: Iterator[(Int, Array[Byte])]
  ): Iterator[Byte] = {
    // This will materialize the entire file in memory,
    // so for very large files this obviously won't work
    // very well. However, for cases where the file is large
    // enough for this to be a problem, it is probably a good idea
    // to use some existing utility to split files before encoding them.

    // TODO Is there any way that we can know how much data needs to be gathered,
    //      so that we can quit early here if we have the full data before
    //      reading all the dataIterator?
    SortedMap.from(indexesAndData).valuesIterator.flatten
  }
}
