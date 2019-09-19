package se.scilifelab.polymerase

import se.scilifelab.reedsolomon.ReedSolomonCoder
import se.scilifelab.reedsolomon.{Defaults => RSDefaults}
import java.nio.ByteBuffer

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

    val dnaEncodedData =
      DNACodec.encodeBlocks(rsEncodedData)

    dnaEncodedData

  }

  def iterateInIndexOrder(in: Iterable[(Int, Array[Byte])]): Iterator[Byte] =
    ???

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
    // TODO This materializes the entire file here, which will not work very well
    //      for large files.
    indexesAndData.toSeq.sortBy(_._1).flatMap(_._2).toIterator
  }

}
