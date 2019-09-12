package se.scilifelab.polymerase

import se.scilifelab.reedsolomon.ReedSolomonCoder
import se.scilifelab.reedsolomon.{Defaults => RSDefaults}

object ReedSolomonDNACodec {

  val rsCoder =
    ReedSolomonCoder(n = RSDefaults.dictonarySize, k = RSDefaults.messageSize)

  def encode(data: Iterator[Byte]): Iterator[Nucleotide] = {

    val groupedDataWithLength = data
      .grouped(RSDefaults.messageSize - Integer.BYTES)
      .map(x => (x.length +: x.map(y => y & (0xff))).toArray)

    val rsEncodedData = groupedDataWithLength.map(mess => rsCoder.encode(mess))

    val dnaEncodedData =
      DNACodec.encode(rsEncodedData.flatMap(x => x.map(_.toByte)))

    dnaEncodedData

  }
  def decode(data: Iterator[Nucleotide]): Iterator[Byte] = ???

}
