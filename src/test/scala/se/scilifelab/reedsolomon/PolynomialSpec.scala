package se.scilifelab.reedsolomon

import org.scalatest._

class PolynomialSpec extends FlatSpec with Matchers {

  import PolynomialImplicits._

  "The Polynomial object" should "add 1" in {
    val one = Polynomial(Array(2.0, 4.0, 7.0, 3.0))
    val two = Polynomial(Array(5.0, 2.0, 4.0, 2.0))

    val result = one + two

    result.cooefficients shouldEqual Array(7, 6, 11, 5)
  }
  it should "add 2" in {
    val one = Polynomial(Array(2.0, 4.0, 7.0, 3.0, 5.0, 2.0))
    val two = Polynomial(Array(5.0, 2.0, 4.0, 2.0))
    val result = one + two

    result.cooefficients shouldEqual Array(2, 4, 12, 5, 9, 4)

  }
  it should "add 3" in {
    val one = Polynomial(Array(7.0, 3.0, 5.0, 2.0))
    val two = Polynomial(Array(6.0, 8.0, 5.0, 2.0, 4.0, 2.0))

    val result = one + two
    result.cooefficients shouldEqual Array(6, 8, 12, 5, 9, 4)
  }

  it should "subtract 1" in {
    val one = Polynomial(Array(7.0, 3.0, 5.0, 2.0))
    val two = Polynomial(Array(6.0, 8.0, 5.0, 2.0, 4.0, 2.0))
    val result = one - two
    result.cooefficients shouldEqual Array(-6, -8, 2, 1, 1, 0)
  }

  it should "multiply 1" in {
    val one = Polynomial(Array(2.0, 4.0, 7.0, 3.0))
    val two = Polynomial(Array(5.0, 2.0, 4.0, 2.0))
    val result = one * two

    result.cooefficients shouldEqual Array(10, 24, 51, 49, 42, 26, 6)
  }

  it should "multiply 2" in {
    val one = Polynomial(Array(2.0, 4.0))
    val two = Polynomial(Array(5.0, 3.0, 1.0))

    val result = one * two

    result.cooefficients shouldEqual Array(10, 26, 14, 4)
  }

  it should "multiply at" in {
    val one = Polynomial(Array(2.0, 4.0, 7.0, 3.0))
    val two = Polynomial(Array(5.0, 2.0, 4.0, 2.0))

    val k = 3
    val result = one * (two, k)
    result shouldEqual 49
  }

  it should "scale" in {
    val one = Polynomial(Array(2.0, 14.0, 7.0, 3.0))
    val scalar = 12

    val result = one.scale(scalar)

    result.cooefficients shouldEqual Array(24, 168, 84, 36)
  }

  it should "do division 1" in {
    val one = Polynomial(Array(1.0, 4.0, 0.0, 3.0))
    val two = Polynomial(Array(1.0, 0.0, 1.0))

    val (q, r) = one / two

    q.cooefficients shouldEqual Array(1, 4)
    r.cooefficients shouldEqual Array(-1, -1)
  }

  it should "do division 2" in {
    val one = Polynomial(Array(1.0, 0.0, 0.0, 2.0, 2.0, 0.0, 1.0, 2.0, 1.0))
    val two = Polynomial(Array(1.0, 0.0, -1.0))

    val (q, r) = one / two

    q.cooefficients shouldEqual Array(1, 0, 1, 2, 3, 2, 4)
    r.cooefficients shouldEqual Array(4, 5)
  }

  it should "do division 3" in {
    val one = Polynomial(Array(1.0, 0.0, -1.0))
    val two = Polynomial(Array(1.0, 1.0, 0.0, -1.0))

    val (q, r) = one / two

    q.cooefficients shouldEqual Array()
    r.cooefficients shouldEqual Array(1, 0, -1)
  }

  it should "do division 4" in {
    val one = Polynomial(Array(1.0, 0.0, 0.0, 2.0, 2.0, 0.0, 1.0, -2.0, -4.0))
    val two = Polynomial(Array(1.0, 0.0, -1.0))

    val (q, r) = one / two

    q.cooefficients shouldEqual Array(1, 0, 1, 2, 3, 2, 4)
    r.isZero should be(true)
  }

  it should "do division my.1" in {
    val one = Polynomial(Array(1.0, 1.0))
    val two = Polynomial(Array(1.0, 1.0))

    val (q, r) = one / two

    q.cooefficients shouldEqual Array(1)
    r.isZero should be(true)
  }

  it should "do division my.2" in {
    val one = Polynomial(Array(1.0, 0.0, 5.0, -4.0))
    val two = Polynomial(Array(1.0, -1.0, 1.0))

    val (q, r) = one / two

    q.cooefficients shouldEqual Array(1, 1)
    r.cooefficients shouldEqual Array(5, -5)
  }

  it should "do division my.4" in {
    val one = Polynomial(Array(1.0, -2.0, 0.0, -4.0))
    val two = Polynomial(Array(1.0, -3.0))

    val (q, r) = one / two

    q.cooefficients shouldEqual Array(1, 1, 3)
    r.cooefficients shouldEqual Array(5)

  }

  it should "evaluate" in {
    val polynomial = Polynomial(Array(5.0, 3.0, 1.0, 1.0, 6.0, 8.0))
    val (inidividualResults, sum) = polynomial.evaluate(3)

    inidividualResults shouldEqual Array(1215, 243, 27, 9, 18, 8)
    sum should be(1520)
  }

  "The Polynomial object" should "handle add GF Polynomials" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val one = Polynomial.fromFiniteField(field)(Array(8, 3, 5, 1))
    val two = Polynomial.fromFiniteField(field)(Array(5, 3, 1, 1, 6, 8))

    val result = one + two
    (two + one) should be(one + two)
    result.cooefficients.map(_.i) shouldEqual Array(5, 3, 9, 2, 3, 9)

  }

  it should "subtract GF Polynomials" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val one = Polynomial.fromFiniteField(field)(Array(8, 3, 5, 1))
    val two = Polynomial.fromFiniteField(field)(Array(5, 3, 1, 1, 6, 8))

    val result = one - two
    result.cooefficients.map(_.i) shouldEqual Array(5, 3, 9, 2, 3, 9)

  }

  it should "multiply GF Polynomials" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val one = Polynomial.fromFiniteField(field)(Array(8, 3, 5, 1))
    val two = Polynomial.fromFiniteField(field)(Array(5, 3, 1, 1, 6, 8))

    val result = one * two
    result.cooefficients.map(_.i) shouldEqual Array(40, 23, 28, 1, 53, 78, 7,
      46, 8)
  }

  it should "do correct multiplication of finite field polynomials (with 0s)" in {

    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)

    val one = Polynomial.fromFiniteField(field)(Array(101, 0, 0))
    val two = Polynomial.fromFiniteField(field)(Array(8, 3, 5, 1))

    val expectedPolynomial =
      Polynomial.fromFiniteField(field)(Array(5, 175, 234, 101, 0, 0))

    (one * two).cooefficients
      .map(_.i) shouldEqual expectedPolynomial.cooefficients.map(_.i)
  }

  it should "scale GF Polynomials" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val one = Polynomial.fromFiniteField(field)(Array(2, 14, 7, 3))
    val k = field.GF2Int(12)

    val result = one.scale(k)
    result.cooefficients.map(_.i) shouldEqual Array(24, 72, 36, 20)
  }

  it should "do division of GF Polynomial" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)

    val one = Polynomial.fromFiniteField(field)(Array(8, 3, 5, 1))
    val two = Polynomial.fromFiniteField(field)(Array(5, 3, 1, 1, 6, 8))

    val (q, r) = two / one

    q.cooefficients.map(_.i) shouldBe Array(101, 152, 11)
    r.cooefficients.map(_.i) shouldBe Array(183, 185, 3)
    ((q * one) + r) should be(two)
  }

  it should "get the correct coefficients" in {

    val p = Polynomial(Array(6.0, 5.0, 4.0, 3.0, 2.0, 1.0))

    p.getCoefficient(0) should be(1.0)
    p.getCoefficient(5) should be(6.0)
    p.getCoefficient(10) should be(0)
  }

  it should "ggFastDivMod" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val x = Polynomial.fromFiniteField(field)(Array(5, 3, 1, 1, 6, 8))
    val y = Polynomial.fromFiniteField(field)(Array(1, 3, 5, 1))

    val (q, r) = x.ggFastDivMod(y)
    q.cooefficients.map(_.i) shouldEqual Array(5, 12, 4)
    r.cooefficients.map(_.i) shouldEqual Array(52, 30, 12)
  }

  it should "ggFastMod" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val x = Polynomial.fromFiniteField(field)(Array(140, 128, 0, 0, 0))
    val y = Polynomial.fromFiniteField(field)(Array(1, 9, 45, 85))

    val r = x.ggFastMod(y)
    r.cooefficients.map(_.i) shouldEqual Array(182, 242, 0)
  }
}
