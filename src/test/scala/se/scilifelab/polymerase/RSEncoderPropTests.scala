package se.scilifelab.polymerase

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object RSEncoderDecoderSpecification
    extends Properties("Polymerase Reed-Solomon Encoders/Decoders") {

  property(
    "Encoding and decoding a stream of bytes should return the original byte"
  ) = forAll { a: Seq[Byte] =>
    a == ReedSolomonDNACodec.decode(ReedSolomonDNACodec.encode(a.toIterator))
  }

}
