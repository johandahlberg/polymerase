package se.scilifelab.polymerase

import org.scalatest._
import scala.annotation.meta.field
import se.scilifelab.polymerase.TestUtils

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import matchers.should.Matchers._
import se.scilifelab.polymerase.UnsignedByte
import se.scilifelab.polymerase.Package

class PackageSpec extends AnyFlatSpec {

  "A package " should "be created correctly" in {
    val pck = Package.fromBytes(0, 1, 5, Array(1.toByte, 2.toByte))
    pck.length shouldBe 2
    pck.index shouldBe 0
    pck.data.map(_.intValue) should be(Array(1, 2))
  }

  it should "get correcty created from raw bytes" in {
    val bytes =
      Array(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2, 1, 2, 0, 0, 0).map(
        x => UnsignedByte(x.toByte)
      )
    val pck = Package.fromRawBytes(bytes)
    pck.totalNumberOfBlocks shouldBe 1
    pck.length shouldBe 2
    pck.index shouldBe 0
    pck.data.map(_.intValue) should be(Array(1, 2))
  }
}
