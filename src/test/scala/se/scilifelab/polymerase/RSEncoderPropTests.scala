package se.scilifelab.polymerase

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen.nonEmptyListOf
import org.scalacheck.Arbitrary.arbitrary

object RSEncoderDecoderSpecification
    extends Properties("Polymerase Reed-Solomon Encoders/Decoders") {

  val nonEmptyBytes = nonEmptyListOf(arbitrary[Byte])

  property(
    "Encoding and decoding a stream of bytes should return the original byte"
  ) = forAll(nonEmptyBytes) { a =>
    val res =
      ReedSolomonDNACodec
        .decode(ReedSolomonDNACodec.encode(a.toIterator))
        .toList
    a == res
  }

}
