package scilifelab.polymerase

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object EncoderDecoderSpecification
    extends Properties("Polymerase Encoders/Decoders") {

  //TODO Write some properties that check the encoding decoding of raw bytes..

  property("Encoding and decoding a Byte should return original Byte") =
    forAll { a: Byte =>
      List(a).toIterable == DNACodec.decode(DNACodec.encode(a))
    }

  property("Encoding a Byte should use half as many bases as bits") = forAll {
    a: Byte =>
      DNACodec.encode(a).toList.length == 4
  }

//  property("encoding and then decoding should return the same data") = forAll {
//    a: String =>
//      println(f"Empty String? ${a.isEmpty()}")
//      a.getBytes == DNACodec.decode(DNACodec.encode(a.getBytes))
//  }

  // Encoded length should be smaller than original byte length...
}
