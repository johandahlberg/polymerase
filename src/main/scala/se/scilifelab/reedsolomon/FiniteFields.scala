package se.scilifelab.reedsolomon

import scala.math.{pow, sqrt}
import scala.annotation.tailrec
import scala.collection.BitSet
import scala.collection.mutable.{BitSet => MutableBitSet}
import util.control.Breaks._

/**
  * A Galois field, is a set of numbers that contains a finite number of elements, and defines operations that
  * you would expect from the a normal set, like integers. For a more formal definition, and more information
  * see wikipedia: https://en.wikipedia.org/wiki/Finite_field
  */
object GaloisField {

  def findPrimePolynomials(
      generator: Int = 2,
      cExp: Int = 8,
      fastPrimes: Boolean = false,
      single: Boolean = false
  ): Array[Int] = {

    val rootCharac = 2
    val fieldCharac: Int = pow(rootCharac, cExp).toInt - 1
    val fieldCharacNext: Int = pow(rootCharac, cExp + 1).toInt - 1

    val primeCandidates: Array[Int] = {
      if (fastPrimes) {
        Utils.primesUpTo(fieldCharacNext).filter(x => x > fieldCharac).toArray
      } else {
        Range.apply(fieldCharac + 2, fieldCharacNext, rootCharac).toArray
      }
    }

    val correctPrimes = MutableBitSet()
    for { prim <- primeCandidates } {
      val seen = MutableBitSet()
      var conflict = false

      var x = GaloisField(
        generator = generator,
        prim = prim,
        gf2CExp = fieldCharac
      ).GF2Int(1)
      breakable {
        for { i <- 0 until fieldCharac } {
          x = x.multiply(generator, prim, fieldCharac + 1)
          if (x.i > fieldCharac || seen(x.i)) {
            conflict = true
            break
          } else {
            seen += x.i
          }
        }
      }

      if (!conflict) {
        correctPrimes += prim
        if (single) {
          return correctPrimes.toArray
        }
      }
    }
    correctPrimes.toArray
  }

}

case class GaloisField(
    generator: Int = 3,
    prim: Int = 0x11b,
    gf2CExp: Int = 8
) {

  val gf2Charac: Int = (pow(2, gf2CExp) - 1).toInt
  val gf2intExpTable: Array[Int] = computeExponentialTable
  val gf2intLogTable: Array[Int] = computeLogTable(gf2intExpTable)

  private def computeExponentialTable = {
    @tailrec
    def comp(
        g: GF2Int,
        expTable: Array[Int],
        c: Int,
        length: Int
    ): Array[Int] = {
      if (c >= length) {
        expTable
      } else {
        val newG = g.multiply(generator, prim, gf2Charac + 1)
        comp(newG, expTable.updated(c, g.i), c + 1, length)
      }
    }
    val initialArray = Array.fill(gf2Charac + 1)(-1)
    comp(GF2Int(1), initialArray, 0, initialArray.length)
  }

  private def computeLogTable(expTable: Array[Int]) = {
    expTable
      .dropRight(1)
      .zipWithIndex
      .foldLeft(Array.fill(gf2Charac + 1)(-1)) {
        case (table, (elem, index)) =>
          table.updated(elem, index)
      }

  }

  object GF2Int {
    implicit val polynomial: PolynomialAritmetics[GF2Int] =
      new PolynomialAritmetics[GF2Int] {
        override def plus(x: GF2Int, y: GF2Int): GF2Int = x + y
        def div(
            x: GF2Int,
            y: GF2Int
        ): GF2Int = x / y

        def fromInt(x: Int): GF2Int = GF2Int(x)
        def minus(
            x: GF2Int,
            y: GF2Int
        ): GF2Int = x - y
        def negate(x: GF2Int): GF2Int = x.negate()
        def times(
            x: GF2Int,
            y: GF2Int
        ): GF2Int = x * y
        def intPow(x: GF2Int, y: Int): GF2Int = x.pow(y)
        def compare(x: GF2Int, y: GF2Int): Int =
          x.i.compare(y.i)
        def zero = GF2Int(0)
        def xor(x: GF2Int, y: GF2Int): GF2Int = x.xor(y)
      }
  }

  case class GF2Int(i: Int) {

    def isZero = i == 0

    def xor(other: GF2Int): GF2Int = GF2Int(this.i ^ other.i)

    def +(other: GF2Int): GF2Int = {
      GF2Int(this.i ^ other.i)
    }

    def -(other: GF2Int): GF2Int = {
      this + other
    }

    def negate(): GF2Int = this

    def *(other: GF2Int): GF2Int = {
      if (this.i == 0 || other.i == 0) {
        GF2Int(0)
      } else {
        val x = gf2intLogTable(this.i)
        val y = gf2intLogTable(other.i)
        val z = (x + y)
        val nextZ = (z & gf2Charac) + (z >> gf2CExp)
        GF2Int(gf2intExpTable(nextZ))
      }
    }

    def pow(power: Int): GF2Int = {
      val x = gf2intLogTable(this.i)
      val z = scala.math.floorMod(x * power, gf2Charac)
      GF2Int(gf2intExpTable(z))
    }

    def inverse: GF2Int = {
      val e = gf2intLogTable(this.i)
      GF2Int(gf2intExpTable(gf2Charac - e))
    }

    def /(other: GF2Int): GF2Int = {
      if (this.i == 0 && other.i == 0) {
        GF2Int(0)
      } else {

        val x = gf2intLogTable(this.i)
        val y = gf2intLogTable(other.i)

        val z = scala.math.floorMod(x - y, gf2Charac)
        GF2Int(gf2intExpTable(z))
      }
    }

    def multiply(
        other: Int,
        prim: Int = 0x11b,
        fieldCharacFull: Int = 256,
        carryLess: Boolean = true
    ): GF2Int = {

      var a = this.i
      var b = other
      var r = 0

      while (b != 0) {
        if ((b & 1) != 0) {
          r = if (carryLess) {
            r ^ a
          } else {
            r + a
          }
        }
        b = b >> 1
        a = a << 1
        if (prim > 0 && (a & fieldCharacFull) != 0) {
          a = a ^ prim
        }
      }
      GF2Int(r)
    }
  }
}
