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

  def decode(data: Iterator[Nucleotide]): Iterator[Byte] = {

    val bytes = DNACodec.decode(data)

    val byteBlocks =
      bytes
        .map(x => x & (0xff))
        .grouped(RSDefaults.dictonarySize)

    byteBlocks
      .map { x =>
        rsCoder.decode(x.toArray)._1
      }
      .flatMap { res =>
        // Only pick up the original data, i.e. strip the length of the data block
        // TODO Get the index and sort based on it.
        val length = res.head
        res.drop(5).take(length).map(_.toByte)
      }
  }

}
