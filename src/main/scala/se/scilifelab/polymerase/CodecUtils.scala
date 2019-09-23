package se.scilifelab.polymerase

import java.nio.ByteBuffer

object CodecUtils {

  def createDataBlock(data: Iterator[Array[Int]]): Iterator[Array[Int]] = {
    data.zipWithIndex
      .map {
        case (x, i) =>
          val index =
            ByteBuffer.allocate(4).putInt(i).array().map(_.toInt)
          val dataLength = x.length + index.length
          val dataAsBytes = (index ++ x).map(_ & 0xff)
          (dataLength +: dataAsBytes).toArray
      }
  }

  def deconstructDataBlock(data: Array[Int]): (Int, Array[Byte]) = {
    val length = data.head
    val index = ByteBuffer.wrap(data.tail.take(4).map(_.toByte)).getInt()
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
    indexesAndData.toSeq.sortBy(_._1).map(_._2).flatten.iterator
  }
}
