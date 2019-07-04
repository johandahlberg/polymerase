package scilifelab.polymerase

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object EncoderDecoderSpecification
    extends Properties("Polymerase Encoders/Decoders") {

  property("encoding and then decoding should return the same data") = forAll {
    a: String =>
      println(f"Empty String? ${a.isEmpty()}")
      a.getBytes == DNACodec.decode(DNACodec.encode(a.getBytes))
  }

  // Encoded length should be smaller than original byte length...
}
