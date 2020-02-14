package se.scilifelab.reedsolomon

import org.scalatest._
import scala.annotation.meta.field
import se.scilifelab.reedsolomon.TestUtils
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import matchers.should.Matchers._

class ReedSolomonSpec extends AnyFlatSpec {

  "The RSCoder object" should "encode 1" in {
    val rsCoder = ReedSolomonCoder(5, 2)

    val mess = Array(140, 128)
    val eccGood = Array(182, 242, 0)

    val mesAndDecc = rsCoder.encode(mess)

    mesAndDecc should be(mess ++ eccGood)

  }

  it should "encode 2" in {
    val rsCoder = ReedSolomonCoder(255, 180)

    val mess = "hello world".map(_.toInt).toArray

    val mesAndDecc = rsCoder.encode(mess).dropWhile(_ == 0)

    val expextedResult =
      Array(104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 52, 234, 152,
        75, 39, 171, 122, 196, 96, 245, 151, 167, 164, 13, 207, 148, 5, 112,
        192, 124, 46, 134, 198, 32, 49, 75, 204, 217, 71, 148, 43, 66, 94, 210,
        201, 128, 80, 185, 30, 219, 33, 53, 174, 183, 121, 191, 69, 203, 2, 206,
        194, 109, 221, 51, 207, 4, 129, 37, 255, 237, 174, 104, 199, 28, 33, 90,
        10, 74, 125, 113, 70, 59, 150, 197, 157)

    mesAndDecc shouldEqual expextedResult

  }

  it should "encoding a vector of zeros does not throw an exception" in {
    val rsCoder = ReedSolomonCoder(255, 180)
    val mesAndDecc = rsCoder
      .encode(Array(0))
  }

  it should "encode an empty vector" in {
    val rsCoder = ReedSolomonCoder(255, 180)
    rsCoder.encode(Array()).isEmpty should be(true)

  }

  it should "check no error" in {
    val rsCoder = ReedSolomonCoder(255, 223)
    val code = rsCoder.encode("Hello World!".map(_.toInt).toArray)

    rsCoder.check(code) should be(true)
  }

  it should "check one error" in {
    val rsCoder = ReedSolomonCoder(255, 223)
    val code = rsCoder.encode(
      "Hello World! This is a test message, to be decoded and verified"
        .map(_.toInt)
        .toArray
    )
    (0 until code.length).foreach {
      case x => {
        val badCode = code.updated(x, {
          if (code(x) == 0) {
            1
          } else {
            0
          }
        })
        rsCoder.checkFast(badCode) should be(false)
      }
    }
  }

  it should "decode messages when there are no errors" in {

    val coder = ReedSolomonCoder(255, 223)
    val message = "Hello, world! This is a long string".map(_.toInt).toArray
    val encodedMessage = coder.encode(message)
    val (decodedMessage, _) = coder.decode(encodedMessage)

    message should be(decodedMessage)

  }

  it should "decode messages (0) when there are no errors" in {

    val coder = ReedSolomonCoder(255, 223)
    val encodedMessage = coder.encode(Array(0))
    val (decodedMessage, _) = coder.decode(encodedMessage)

    encodedMessage.count(x => x == 0) should be(255)

  }

  it should "encode decode message Array(202, 130, 61, 105, 255, 45, 102, 94, 180, 90)" in {
    val mess = Array(202, 130, 61, 105, 255, 45, 102, 94, 180, 90)

    val coder = ReedSolomonCoder(255, 223)
    val encodedMessage = coder.encode(mess)

    val (decodedMessage, _) = coder.decode(encodedMessage)
    decodedMessage shouldEqual mess
  }

  it should "decode messages (when there is one error)" in {
    val coder = ReedSolomonCoder(255, 223)
    val message = "Hello, world! This is a long string".map(_.toInt).toArray
    val encodedMessage = coder.encode(message)

    encodedMessage.zipWithIndex.map {
      case (value: Int, index: Int) => {
        val garbledMessage = encodedMessage.updated(
          index,
          scala.math.floorMod(encodedMessage(index) + 50, 256)
        )
        val (decodedMessage, _) = coder.decode(garbledMessage)
        message should be(decodedMessage)
      }
    }
  }

  it should "decode zero message when there are half as many errors as correctors" in {
    val coder = ReedSolomonCoder(100, 90)
    val message = Array(0)
    val encodedMessage = coder.encode(message)

    val garbledMessage = TestUtils.corruptMessage(encodedMessage, 5)
    val (decodedMessage, _) = coder.decode(garbledMessage, noStrip = true)

    decodedMessage.count(x => x == 0) should be(90)
  }

  it should "decode specific seq (problem example for previous property testing round)" in {

    val mess = Array(207, 59, 27, 134, 192, 101, 105, 30, 26, 204, 211, 114, 67,
      61, 242, 115, 246, 40, 235, 121, 56, 15, 253, 130, 176, 49, 73, 2, 196,
      166, 76, 75, 232, 87, 6, 223, 195, 144, 244, 222, 105, 187, 98, 69, 19,
      219, 1, 108, 117, 226, 48, 65, 4, 87, 106, 189, 111, 6, 77, 121, 245, 112,
      2, 45, 7, 24, 155, 180, 146, 10, 100, 56, 142, 15, 251, 220, 59, 195, 104,
      22, 115, 105, 26, 136, 218, 225, 2, 227, 235, 40, 14, 131, 154, 191)

    val coder = ReedSolomonCoder(120, 110)
    val encodedMessage = coder.encode(mess)

    val garbledMessage = TestUtils.corruptMessage(encodedMessage, 5, 123)
    val (decodedMessage, _) = coder.decode(garbledMessage)

    decodedMessage should be(mess)
  }

  it should "decode fixed message (when there is one error)" in {
    val coder = ReedSolomonCoder(255, 223)
    val message = Range(1, 21).toArray
    val encodedMessage = coder.encode(message)

    val garbledMessage = TestUtils.corruptMessage(encodedMessage, 1)
    val (decodedMessage, _) = coder.decode(garbledMessage)
    message.toSeq should be(decodedMessage.toSeq)
  }

  it should "decode an empty message" in {
    val coder = ReedSolomonCoder(255, 223)
    val (mess, corr) = coder.decode(Array())
    mess.isEmpty should be(true)
    corr.isEmpty should be(true)
  }

  it should "do correct Berlekamp-Massy decoding" in {

    val coder = ReedSolomonCoder(255, 223)
    val (resSigma, resOmega) = coder.berlekampMassey(
      Polynomial.fromFiniteField(coder.field)(Array(1, 2, 3, 4, 5))
    )
    resSigma.cooefficients shouldEqual Array(coder.field.GF2Int(1))
    resOmega.cooefficients shouldEqual Array(coder.field.GF2Int(1))

  }

  it should "calculate corret syndromes" in {

    val originalMessage = "hello world".map(_.toInt).toArray
    val n = originalMessage.length * 2
    val k = originalMessage.length

    val fcr = 1
    val prim = 0xfd
    val generator = 3
    val cExponent = 7

    val erasenb = 5

    val rsMan = ReedSolomonCoder(
      n,
      k,
      fcr = fcr,
      prim = prim,
      generator = generator,
      cExp = cExponent
    )
    val rmesecc = rsMan.encode(originalMessage)
    val rmececcUpdated =
      rmesecc.patch(rmesecc.length - erasenb, Array.fill(erasenb)(0), erasenb)

    val poly = Polynomial.fromFiniteField(rsMan.field)(rmececcUpdated)
    val synd = rsMan.syndromes(poly)

    synd.cooefficients.map(_.i) shouldEqual Array(0, 75, 112, 28, 95, 77, 73,
      113, 22, 29, 100, 0)
    synd.cooefficients.length should be(12)

  }

}
