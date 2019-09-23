package se.scilifelab.polymerase

import se.scilifelab.reedsolomon.ReedSolomonCoder
import se.scilifelab.reedsolomon.{Defaults => RSDefaults}

object ReedSolomonDNACodec {

  // TODO These settings generate to long DNA sequences
  // per block. Look at better values.
  private val rsCoder =
    ReedSolomonCoder(n = RSDefaults.dictonarySize, k = RSDefaults.messageSize)

  lazy val writeBlockSize: Int = RSDefaults.messageSize - (Integer.BYTES * 2)
  lazy val readBlockSize: Int = RSDefaults.dictonarySize

  // TODO Psudo randomize bytes, to avoid monomer stretches.
  //      Or better still, rotate bases as described in X.

  def encodeBlock(data: Iterator[Array[Int]]): Iterator[Array[Nucleotide]] = {

    val groupedDataWithLength = CodecUtils.createDataBlock(data)

    val rsEncodedData = groupedDataWithLength.map { mess =>
      rsCoder.encode(mess)
    }

    val dnaEncodedData =
      rsEncodedData.map(data => DNACodec.encode(data))

    dnaEncodedData

  }

  def encode(data: Iterator[Byte]): Iterator[Array[Nucleotide]] = {
    encodeBlock(
      data
      // Use the integers to encode length and index of message
        .grouped(ReedSolomonDNACodec.writeBlockSize)
        .map(_.map(_ & 0xff).toArray)
    )

  }

  def decode(data: Iterator[Array[Nucleotide]]): Iterator[Byte] = {

    val bytes = data.map(d => DNACodec.decode(d.iterator)).flatten

    val byteBlocks =
      bytes
        .map(x => x & (0xff))
        .grouped(ReedSolomonDNACodec.readBlockSize)

    val indexesAndData = byteBlocks
      .map { x =>
        rsCoder.decode(x.toArray)._1
      }
      .map { res =>
        CodecUtils.deconstructDataBlock(res)
      }

    CodecUtils.sortIndexAndDataOutput(indexesAndData)
  }

}
