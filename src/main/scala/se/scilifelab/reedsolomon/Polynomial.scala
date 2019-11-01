package se.scilifelab.reedsolomon

import scala.annotation.tailrec
import scala.annotation.meta.field
import scala.reflect.ClassTag

/**
  * Trait that needs to be implemented to provide aritmetics for polynomial coefficients
  */
trait PolynomialAritmetics[T] {
  def plus(x: T, y: T): T
  def div(x: T, y: T): T
  def minus(x: T, y: T): T
  def negate(x: T): T
  def times(x: T, y: T): T
  def intPow(x: T, y: Int): T
  def compare(x: T, y: T): Int
  def zero: T
  def xor(x: T, y: T): T
}

/**
  * PolynomialImplicits contain the methods necessary to do polynomial aritmetics for some simple
  * numeric types, e.g. integers and doubles
  */
object PolynomialImplicits {

  implicit val intPolynomialAritmetics: PolynomialAritmetics[Int] =
    new PolynomialAritmetics[Int] {
      def plus(x: Int, y: Int): Int = x + y
      def div(
          x: Int,
          y: Int
      ): Int = x / y

      def fromInt(x: Int): Int = x
      def minus(
          x: Int,
          y: Int
      ): Int = x - y
      def negate(x: Int): Int = -x
      def times(
          x: Int,
          y: Int
      ): Int = x * y
      def intPow(x: Int, y: Int): Int =
        scala.math.pow(x.toDouble, y.toDouble).toInt
      def compare(x: Int, y: Int): Int =
        x.compare(y)
      def zero: Int = 0
      def xor(x: Int, y: Int): Int = x ^ y
    }

  implicit val doublePolynomialAritmetics: PolynomialAritmetics[Double] =
    new PolynomialAritmetics[Double] {
      def plus(x: Double, y: Double): Double = x + y
      def div(
          x: Double,
          y: Double
      ): Double = x / y

      def fromDouble(x: Double): Double = x
      def minus(
          x: Double,
          y: Double
      ): Double = x - y
      def negate(x: Double): Double = -x
      def times(
          x: Double,
          y: Double
      ): Double = x * y
      def intPow(x: Double, y: Int): Double =
        scala.math.pow(x, y).toDouble
      def compare(x: Double, y: Double): Int =
        x.compare(y)
      def zero: Double = 0
      def xor(x: Double, y: Double): Double = ???
    }
}

/**
  *  Represents a Polynomial for the type T i.e. for T=Int: y = 3x^3 + x + 1
  *
  * It requires a typeclass to be passed (implicitly or expliclity) that provides
  * polynomial artimetics, i.e. rules for how to carry out mathematical opertions on the coefficents
  * when doing calculations on the polynomial.
  */
case class Polynomial[T: ClassTag](
    inputCoefficients: Array[T],
    keepZeros: Boolean = false
)(implicit polynomialAritmetics: PolynomialAritmetics[T]) {

  // Remove leading zeros from input coefficients.
  val cooefficients = if (keepZeros) {
    inputCoefficients
  } else {
    inputCoefficients
      .dropWhile(x => x == polynomialAritmetics.zero)
  }
  val length = cooefficients.length
  val degree = this.length - 1

  override def equals(other: Any): Boolean = {
    other match {
      case Polynomial(inputCoefficients, keepZeros) =>
        this.inputCoefficients.toSeq == inputCoefficients.toSeq
    }
  }

  def +(other: Polynomial[T]): Polynomial[T] = {
    val l: Array[T] =
      this.cooefficients.reverse
        .zipAll(
          other.cooefficients.reverse,
          polynomialAritmetics.zero,
          polynomialAritmetics.zero
        )
        .map {
          case (x, y) => {
            polynomialAritmetics.plus(x, y)
          }
        }
    Polynomial[T](l.reverse)
  }

  def *(other: Polynomial[T]): Polynomial[T] = {
    val newCoefficientsAndDegrees =
      for {
        (coeff1, degree1) <- this.cooefficients.zipWithIndex
        (coeff2, degree2) <- other.cooefficients.zipWithIndex
      } yield {
        (
          polynomialAritmetics.times(coeff1, coeff2),
          degree1 + degree2
        )
      }
    val simlified =
      newCoefficientsAndDegrees
        .groupBy(x => x._2)
        .mapValues(x => x.map(y => y._1))
        .mapValues(x => x.reduce { polynomialAritmetics.plus })
        .toArray
        .sortBy(_._1)
        .map(_._2)
    Polynomial[T](simlified)
  }

  def *(other: Polynomial[T], at: Int): T = {
    (this * other).getCoefficient(at)
  }

  def -(other: Polynomial[T]): Polynomial[T] = {
    this + Polynomial[T](
      other.cooefficients.map(
        x => polynomialAritmetics.negate(x)
      )
    )
  }

  def /(divisor: Polynomial[T]): (Polynomial[T], Polynomial[T]) = {
    return Polynomial.division(this, divisor, Polynomial())
  }

  def scale(scalar: T): Polynomial[T] = {
    Polynomial[T](
      this.cooefficients
        .map(x => polynomialAritmetics.times(x, scalar))
    )
  }

  def %(other: Polynomial[T]): Polynomial[T] =
    (this / other)._2

  def ggFastMod(
      divisor: Polynomial[T]
  ): Polynomial[T] = {
    this.ggFastDivMod(divisor)._2
  }

  def ggFastDivMod(
      divisor: Polynomial[T]
  ): (Polynomial[T], Polynomial[T]) = {
    // Note! This can only be used with GF2Int
    val dividend = this

    val resPolynomial =
      (0 until (dividend.length - (divisor.length - 1))).foldLeft(dividend) {
        case (res, i) => {
          val coef = res.cooefficients(i)
          if (coef != polynomialAritmetics.zero) {
            (1 until divisor.length).foldLeft(res) {
              case (innerRes, j) =>
                val v = innerRes.cooefficients(i + j)
                val mul =
                  polynomialAritmetics.times(divisor.cooefficients(j), coef)
                val tmp = polynomialAritmetics.xor(v, mul)
                Polynomial(innerRes.cooefficients.updated(i + j, tmp))
            }
          } else {
            res
          }
        }
      }

    val separator = divisor.degree
    val (q, r) = resPolynomial.cooefficients.splitAt(separator)
    (
      Polynomial(resPolynomial.cooefficients.take(separator)),
      Polynomial(resPolynomial.cooefficients.takeRight(separator))
    )
  }

  override def toString: String = {
    val inner = this.cooefficients.reverse.zipWithIndex
      .filter {
        case (coeff, degree) =>
          coeff != polynomialAritmetics.zero
      }
      .map {
        case (coeff, 0)      => coeff
        case (coeff, degree) => s"$coeff*x^$degree"
      }
      .mkString(" + ")
    if (inner.isEmpty()) {
      "Empty Polynomial"
    } else {
      s"Polynomial($inner)"
    }
  }

  def lead(): T = {
    getCoefficient(0)
  }

  def getCoefficient(term: Int): T = {
    // Get the coefficient of the specified term, no of the index!
    if (term < length) {
      this.cooefficients.reverse(term)
    } else {
      polynomialAritmetics.zero
    }
  }

  def evaluate(x: T): (Array[T], T) = {
    val results = this.cooefficients.reverse.zipWithIndex.map {
      case (y, 0) =>
        y
      case (coeff, degree) =>
        val power = polynomialAritmetics.intPow(x, degree)
        polynomialAritmetics
          .times(coeff, power)
    }
    if (results.isEmpty) {
      (results.reverse, polynomialAritmetics.zero)
    } else {
      (
        results.reverse,
        results.reduce(polynomialAritmetics.plus)
      )
    }
  }

  def isZero =
    !cooefficients.exists(x => x != polynomialAritmetics.zero)

  def set(index: Int, elem: T): Polynomial[T] =
    if (index < this.length) {
      Polynomial[T](this.cooefficients.updated(index, elem))
    } else {
      Polynomial[T](
        elem +: Array
          .fill(index - this.length)(polynomialAritmetics.zero) :++ this.cooefficients
      )
    }
}

object Polynomial {

  /**
    * Create an empty polynomial
    */
  def apply[T: PolynomialAritmetics: ClassTag](): Polynomial[T] =
    Polynomial[T](Array())

  /**
    * Utility method to cheate a polynomial where the coefficients are
    * elements in a GaloisField (i.e. a fintite field). Useful when you have the fields
    * and a set of coefficients.
    */
  def fromFiniteField(
      field: GaloisField
  )(coefficients: Array[Int]) = {
    Polynomial(coefficients.map { x =>
      field.GF2Int(x)
    })
  }

  @tailrec
  protected[reedsolomon] final def division[T: ClassTag](
      dividend: Polynomial[T],
      divisor: Polynomial[T],
      quotient: Polynomial[T]
  )(
      implicit polynomialAritmetics: PolynomialAritmetics[T]
  ): (Polynomial[T], Polynomial[T]) = {

    val dividendPower = dividend.degree
    val dividendCoefficient = dividend.getCoefficient(dividend.degree)

    val divisorPower = divisor.degree
    val divisorCoefficient = divisor.getCoefficient(divisor.degree)

    val quotientPower = dividendPower - divisorPower

    if (quotientPower < 0) {
      (quotient, dividend)
    } else {
      val quotientCoefficient =
        polynomialAritmetics
          .div(dividendCoefficient, divisorCoefficient)
      val newQuotient = Polynomial[T](
        Array(quotientCoefficient) ++ Array.fill(quotientPower)(
          polynomialAritmetics.zero
        )
      )
      val remainder = dividend - (divisor * newQuotient)

      if (remainder.isZero) {
        (quotient + newQuotient, remainder)
      } else {
        division(remainder, divisor, quotient + newQuotient)
      }
    }
  }
}
