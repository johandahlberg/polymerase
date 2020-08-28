package se.scilifelab.polymerase

import se.scilifelab.polymerase.fountaincodes.FountainsCodes

object FountainCodesDNACodec {}
//
//  // TODO Make seed configurable
//  val randomSeed = 1234
//  val fountainCoder = new FountainsCodes(randomSeed)
//
//  // TODO Make the block size configurable
//  val blockSize = 10
//
//  def encode(data: Iterator[UnencodedPackage]): Iterator[Array[Nucleotide]] = {
//
//    val blocks =
//      CodecUtils
//        .createDataBlock(data)
//
//    //     .grouped(blockSize)
//    //     .flatMap { x =>
//    //       x.map(_.data)
//    //     }
//    //     .map { x =>
//    //       (CodecUtils.encodeIntAsBytes(x.length) ++ x
//    //         .map(_.underlyingByte)
//    //         .toSeq).toSeq
//    //     }
//    //     .toSeq
//
//    // val fountainEncodedData =
//    //   fountainCoder.encode(blocks).map(symbol => symbol.data.toArray)
//    // DNACodec.encodeByteBlocks(fountainEncodedData)
//  }
//
//  def decode(data: Iterator[Array[Nucleotide]]): Iterator[UnencodedPackage] = {
//    val decodedFromDNA = data.map(datum => DNACodec.decode(datum.iterator))
//    fountainCoder.decode()
//
//    ???
//  }
//
//}
