package net.alasc.symdpoly
package generic

import shapeless.Witness
import spire.algebra.{Eq, Order, VectorSpace}
import spire.math.Rational
import spire.syntax.cfor._

import cyclo.{Cyclo, RealCyclo}

import net.alasc.finite.Grp
import scalin.immutable.{Vec, VecEngine}
import scalin.syntax.all._
import spire.std.double._
import spire.syntax.vectorSpace._
import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.symdpoly.sdp.BasisTerm
import net.alasc.symdpoly.util.OrderedSet
import spire.syntax.involution._

/** Evaluated noncommutative polynomial. */
final class LinearMoment[
  E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
  M <: generic.MonoDef with Singleton:Witness.Aux
](val normalForm: M#PolyType) extends LinearMomentLike[E, M] {
  lhs: E#LinearMomentType =>

  def M: M = valueOf[M]

  def E: E = valueOf[E]

  def isZero: Boolean = normalForm.isZero

  def toPoly: LinearMoment[E, M] = lhs

  override def toString: String = normalForm.string("L(", ")")

  override def hashCode: Int = normalForm.hashCode()

  override def equals(any: Any): Boolean = any match {
    case rhs: LinearMoment[E, M] if (lhs.E eq rhs.E) => valueOf[M].polyEq.eqv(lhs.normalForm, rhs.normalForm)
    case _ => false
  }

  def nTerms: Int = normalForm.nTerms

  def monomial(i: Int): SingleMoment[E, M] = new SingleMoment[E, M](normalForm.monomial(i))

  def coeff(i: Int): Cyclo = normalForm.coeff(i)

  def coeff(mono: SingleMoment[E, M]): Cyclo = normalForm.coeff(mono.normalForm)

  def vectorOverOrderedSet(orderedSet: OrderedSet[SingleMoment[E, M]])(implicit V: VecEngine[Cyclo]): Vec[Cyclo] =
    V.fromMutable(orderedSet.length, Cyclo.zero) { vec =>
      cforRange(0 until nTerms) { i =>
        val m = monomial(i)
        val c = coeff(i)
        val j = orderedSet.indexOf(m)
        if (j == -1) sys.error(s"Monomial $m not present in the moment set ${orderedSet}.") else { vec(j) := vec(j) + c }
      }
    }

  def invariantSubgroupOf(grp: Grp[M#PermutationType]): Grp[M#PermutationType] =
    symmetries.invariantSubgroupOf[E#SingleMomentType, M#PermutationType]((0 until nTerms).map(monomial), (x: SingleMoment[E, M]) => coeff(x), E.compatibleSubgroup(grp), M.cyclotomicOrder)

  /** Expands this evaluated polynomial in the given SDP relaxation.
    *
    * Returns a vector v such that the product v * y gives the value of this evaluated polynomial, with
    * y the real variables of the SDP relaxation.
    */
  def vectorSelfAdjointIn(relaxation: Relaxation[E, M]): Vec[Double] = {
    import relaxation._
    import scalin.immutable.dense._
    Vec.fromMutable(allMoments.length, 0.0) { vec =>
      cforRange(0 until nTerms) { pi =>
        val m = monomial(pi)
        val i = allMoments.indexOf(m)
        val iadj = adjointMoment(i)
        val beta = coeff(pi)
        if (i == iadj) {
          assert(beta.isReal)
          vec(i) := vec(i) + cycloValue(beta).real
        } else if (i < iadj) { // only consider half the self-adjoint terms
          // we have beta <m> + beta' <m'> =
          //   beta.real * m.real - beta.imag * m.imag + i * (beta.real * m.imag + beta.imag * m.real)
          //   beta.real * m.real - beta.imag * m.imag - i * (beta.real * m.imag + beta.imag * m.real)
          // = 2 * beta.real*m.real - 2 * beta.imag*m.imag
          val madj = allMoments(iadj)
          val betaAdj = coeff(madj)
          assert(betaAdj.conjugate === beta)
          vec(i) := vec(i) + cycloValue(beta).real * 2
          vec(iadj) := vec(iadj) - cycloValue(beta).imag * 2
        }
      }
    }
  }

  /** Expands this polynomial in the given SDP relaxation such that
    * real(L(poly)) = sum_i terms(i).realPart * y(terms(i).basisIndex))
    * imag(L(poly)) = sum_i terms(i).imagPart * y(terms(i).basisIndex))
    */
  def expandIn(relaxation: Relaxation[E, M]): Seq[BasisTerm] =
    (0 until nTerms).flatMap(i => monomial(i).expandIn(relaxation, coeff(i))).collapse

  def vectorRealImagPartsIn(relaxation: Relaxation[E, M]): Seq[Vec[Double]] = {
    implicit def polyVectorSpace: VectorSpace[E#LinearMomentType, Cyclo] = E.evaluatedPolyVectorSpace
    val adj = lhs.adjoint
    val realPart = (lhs + adj) :* (Cyclo.one / 2)
    val imagPart  = (lhs - adj) :* (-Cyclo.i / 2)
    Seq(realPart, imagPart).filterNot(_.isZero).map(_.vectorSelfAdjointIn(relaxation))
  }

}

object LinearMoment {

  implicit def polyFromInt[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoDef with Singleton:Witness.Aux
  ](i: Int): LinearMoment[E, M] = valueOf[E].apply(valueOf[M].polyAssociativeAlgebra.fromInt(i))

  implicit def polyFromRational[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoDef with Singleton:Witness.Aux
  ](r: Rational): LinearMoment[E, M] = valueOf[E].apply(valueOf[M].polyAssociativeAlgebra.timesl(Cyclo.viewFromRational(r), valueOf[M].polyAssociativeAlgebra.one))

  implicit def polyFromCyclo[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoDef with Singleton:Witness.Aux
  ](c: Cyclo): LinearMoment[E, M] = valueOf[E].apply(valueOf[M].polyAssociativeAlgebra.timesl(c, valueOf[M].polyAssociativeAlgebra.one))

  //region Typeclasses

  implicit def equ[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoDef with Singleton
  ]: Eq[LinearMoment[E, M]] = valueOf[E].evaluatedPolyEq

  implicit def vectorSpace[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoDef with Singleton
  ]: VectorSpace[LinearMoment[E, M], Cyclo] = valueOf[E].evaluatedPolyVectorSpace

  //endregion

}
