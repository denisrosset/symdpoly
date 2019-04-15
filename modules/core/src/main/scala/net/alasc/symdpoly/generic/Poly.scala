package net.alasc.symdpoly
package generic

import shapeless.Witness
import spire.math.Rational
import spire.syntax.action._

import syntax.phased._
import cyclo.Cyclo
import spire.syntax.cfor._
import spire.syntax.group._
import spire.syntax.ring._
import net.alasc.util._
import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.symdpoly.math.Phase
import net.alasc.symdpoly.util.OrderedSet
import syntax.phased._
import net.alasc.perms.default._

/** Mixin trait for types that can be converted to a M#PolyType in the monoid M.
  *
  * For example, monomials are also polynomials with a single term.
  *
  * However, Scala has a static type system, and we need to leave the ambiguities in the syntax
  * (nobody wants to explicitly convert monomials to polynomials when required), and resolve these ambiguities
  * at compile time.
  *
  * We use the following approach.
  *
  * All types that can be embedded in a `Poly` extend a trait `PolyLike`.
  * The trait `PolyLike` defines all methods that need to 'upgrade' the return value to `Poly`,
  * for example addition or subtraction.
  *
  * Primitive types such as `Int` cannot be extended in this way, and the multiple overloads of `+`
  * conflict with our conversions. Thus, we add to `PolyLike` the method variants that take an `Int`,
  * `Rational` or `Cyclo` right hand side. This allows expressions such as `mono + 1` .
  *
  * This trait defines all the methods where we are sure that the return type is M#PolyType.
  */
trait PolyLike[M <: MonoDef with Singleton] { lhs =>
  def M: M

  // to implement

  def toPoly: M#PolyType
  def isZero: Boolean = M.polyAssociativeAlgebra.isZero(toPoly)(M.polyEq)

  // operations involving polynomials, or polynomial-like objects

  // here, the argument is M#PolyType, because that's only in that case that we return a polynomial
  // otherwise, when the rhs is a monomial, the return type is a monomial
  def *(rhs: M#PolyType): M#PolyType = M.polyAssociativeAlgebra.times(lhs.toPoly, rhs)
  def +(rhs: PolyLike[M]): M#PolyType = M.polyAssociativeAlgebra.plus(lhs.toPoly, rhs.toPoly)
  def -(rhs: PolyLike[M]): M#PolyType = M.polyAssociativeAlgebra.minus(lhs.toPoly, rhs.toPoly)

  // scalar addition or subtraction

  def +(rhs: Int): M#PolyType = lhs.toPoly + constant(Cyclo(rhs))
  def +(rhs: Rational): M#PolyType = lhs.toPoly + constant(rhs)
  def +(rhs: Phase)(implicit d: DummyImplicit): M#PolyType = lhs.toPoly + constant(rhs)
  def +(rhs: Cyclo): M#PolyType = lhs.toPoly + constant(rhs)

  def -(rhs: Int): M#PolyType = lhs.toPoly - constant(Cyclo(rhs))
  def -(rhs: Rational): M#PolyType = lhs.toPoly - constant(rhs)
  def -(rhs: Phase)(implicit d: DummyImplicit): M#PolyType = lhs.toPoly - constant(rhs)
  def -(rhs: Cyclo): M#PolyType = lhs.toPoly - constant(rhs)

  // scalar multiplication or division

  def *(rhs: Int): M#PolyType = lhs.toPoly * constant(Cyclo(rhs))
  def *(rhs: Rational): M#PolyType = lhs.toPoly * constant(rhs: Cyclo)
  def *(rhs: Cyclo): M#PolyType = lhs.toPoly * constant(rhs)

  def /(rhs: Int): M#PolyType = lhs * Cyclo(rhs).reciprocal
  def /(rhs: Rational): M#PolyType = lhs * rhs.reciprocal
  def /(rhs: Cyclo): M#PolyType = lhs * rhs.reciprocal

  def constant(c: Cyclo): M#PolyType = {
    val alg = M.polyAssociativeAlgebra
    if (c.isZero) alg.zero
    else if (c.isOne) alg.one
    else alg.timesr(alg.one, c)
  }

}

object PolyLike {

  implicit def toPoly[M <: MonoDef with Singleton](polyLike: PolyLike[M]): M#PolyType = polyLike.toPoly

}

/** Polynomial written as a linear combination of monomials described by the monoid M. */
abstract class Poly[M <: generic.MonoDef with Singleton] extends PolyLike[M] { lhs: M#PolyType =>
  def toPoly: M#PolyType = lhs

  def nTerms: Int
  def monomial(i: Int): M#MonoType
  def coeff(i: Int): Cyclo
  def coeff(mono: M#MonoType): Cyclo

  def degree: Int

  def string(leftBracket: String = "", rightBracket: String = ""): String

  def invariantSubgroupOf(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = {
    implicit def wM: Witness.Aux[M] = M.witness
    symmetries.invariantSubgroupOf((0 until nTerms).map(monomial), (x: M#MonoType) => coeff(x), grp, M.cyclotomicOrder)
  }

  def *(rhs: Phase)(implicit d: DummyImplicit): Poly[M] = M.polyAssociativeAlgebra.timesr(lhs, rhs.toCyclo)
  def /(rhs: Phase)(implicit d: DummyImplicit): Poly[M] = M.polyAssociativeAlgebra.timesr(lhs, rhs.reciprocal.toCyclo)

}
