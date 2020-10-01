package se.scilifelab.polymerase

import scala.util.Random
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest._
import matchers.should.Matchers._
import org.scalacheck.Arbitrary
import se.scilifelab.polymerase.UnsignedByte
import se.scilifelab.polymerase.Package
import se.scilifelab.polymerase.TestUtils

class FountainCodesPropertySpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks {

  // This disables shrinking, which seems to cause more harm than
  // good here.
  import org.scalacheck.Shrink.shrinkAny

  val blockLength = 5
  val dataPackages = Gen
    .nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(TestUtils.createUnencodedDataPackage(_, blockLength))

  "The PackageDNACodec" should "encode and decode any message" in {
    forAll(dataPackages) { packages =>
      {
        val encoded = PackageDNACodec.encode(packages.iterator)
        val res = PackageDNACodec.decode(encoded)

        packages.map(_.data.toList) should equal(res.map(_.data.toList).toList)
      }
    }
  }

}
