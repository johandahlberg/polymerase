package se.scilifelab.polymerase

import org.scalatest._

class ReedSolomonDNACodecSpec extends FlatSpec with Matchers {

  "The RSDNACodec" should "encode return empty iterator for empty data" in {
    ReedSolomonDNACodec.encode(List().toIterator) === Iterator.empty
  }

  it should "encode small list of bytes" in {
    val data = Seq(37.toByte)
    val encoded = ReedSolomonDNACodec.encode(data.toIterator).toSeq
    val decoded = ReedSolomonDNACodec.decode(encoded.iterator).toSeq
    decoded === data
  }

  it should "encode small String" in {
    val data = Seq("37".toByte)
    ReedSolomonDNACodec
      .decode(ReedSolomonDNACodec.encode(data.toIterator))
      .toSeq === data
  }
}
