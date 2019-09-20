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

  def encode(data: Iterator[Byte]): Iterator[Array[Nucleotide]] = {

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
      rsEncodedData.map(data => DNACodec.encode(data))

    dnaEncodedData

  }

  def decode(data: Iterator[Array[Nucleotide]]): Iterator[Byte] = {

    val bytes = data.map(d => DNACodec.decode(d.iterator)).flatten

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
    // This will materialize the entire file in memory,
    // so for very large files this obviously won't work
    // very well. However, for cases where the file is large
    // enough for this to be a problem, it is probably a good idea
    // to use some existing utility to split files before encoding them.
    indexesAndData.toSeq.sortBy(_._1).map(_._2).flatten.iterator
  }

}
