package se.scilifelab.polymerase

import se.scilifelab.reedsolomon.ReedSolomonCoder
import se.scilifelab.reedsolomon.{Defaults => RSDefaults}

object ReedSolomonDNACodec {

  lazy val messageLength = 80
  lazy val codeWordLength = 90

  // TODO These settings generate to long DNA sequences
  // per block. Look at better values.
  private val rsCoder =
    ReedSolomonCoder(n = messageLength, k = codeWordLength)

  // TODO Psudo randomize bytes, to avoid monomer stretches.
  //      Or better still, rotate bases as described in X.

  def encode(data: Iterator[Byte]): Iterator[Nucleotide] = {

    val groupedDataWithLength = data
      .grouped(messageLength - Integer.BYTES)
      .map(x => (x.length +: x.map(y => y & (0xff))))

    val rsEncodedData = groupedDataWithLength.map { mess =>
      rsCoder.encode(mess.toArray)
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
        // There is 8 bits per encoded byte, and 2 bits encoded
        // per nucleotide.
        // 8 / 2 = 4 which is the number modifer for the message size.
        .grouped(messageLength * 4)

    byteBlocks
      .map { x =>
        rsCoder.decode(x.toArray)._1
      }
      .flatMap { res =>
        // Only pick up the original data, i.e. strip the length of the data block
        val length = res.head
        res.tail.take(length).map(_.toByte)
      }
  }

}
