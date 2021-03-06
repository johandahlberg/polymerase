package se.scilifelab.polymerase

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen.{nonEmptyListOf, listOfN}
import org.scalacheck.Arbitrary.arbitrary

object RSEncoderDecoderSpecification
    extends Properties("Polymerase Reed-Solomon Encoders/Decoders") {

  val nonEmptyBytes = nonEmptyListOf(arbitrary[Byte])

  property(
    "Encoding and decoding a stream of bytes should return " +
      "the original byte when there are no errors"
  ) = forAll(nonEmptyBytes) { a =>
    val res =
      ReedSolomonDNACodec
        .decode(ReedSolomonDNACodec.encode(a.toIterator))
        .toList
    a == res
  }

  property(
    "Encoding and decoding a stream of bytes should return " +
      "the original byte when there are are errors"
  ) = forAll(nonEmptyBytes) { a =>
    val encoded = ReedSolomonDNACodec.encode(a.toIterator).toList
    val garbled = encoded.map(x => x.updated(x.indexOf('A'), 'T'))
    require(encoded != garbled)
    val res = ReedSolomonDNACodec.decode(garbled.toIterator).toList
    a == res
  }

  property(
    "Encoding and decoding a stream of bytes should return " +
      "the original byte when there are are errors and duplications of " +
      "data"
  ) = forAll(nonEmptyBytes) { a =>
    val encoded = ReedSolomonDNACodec.encode(a.toIterator).toList
    val garbled = encoded.map(x => x.updated(x.indexOf('A'), 'T'))
    val garbledAndDuplicated = garbled ::: garbled
    require(encoded != garbledAndDuplicated)
    val res = ReedSolomonDNACodec.decode(garbledAndDuplicated.toIterator).toList
    a == res
  }

}
