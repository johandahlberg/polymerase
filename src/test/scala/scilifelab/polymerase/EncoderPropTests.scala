package scilifelab.polymerase

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object EncoderDecoderSpecification
    extends Properties("Polymerase Encoders/Decoders") {

  property("Encoding and decoding a Byte should return original Byte") =
    forAll { a: Byte =>
      a == DNACodec.decode(DNACodec.encode(a).toIterator).next()
    }

  property(
    "Encoding and decoding a Array[Byte] should return original Array[Byte]"
  ) = forAll { a: List[Byte] =>
    a == DNACodec.decode(DNACodec.encode(a.iterator)).toList
  }

  property("Encoding and decodig a String should return the original String") =
    forAll { a: String =>
      a == DNACodec.decodeString(DNACodec.encode(a))
    }

  property("Encoding a Byte should use half as many bases as bits") = forAll {
    a: Byte =>
      DNACodec.encode(a).toList.length == 4
  }
}
