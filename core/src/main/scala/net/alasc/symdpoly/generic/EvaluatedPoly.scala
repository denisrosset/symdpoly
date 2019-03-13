package net.alasc.symdpoly
package generic

import shapeless.Witness
import spire.algebra.{Eq, Order, VectorSpace}
import spire.math.Rational
import spire.syntax.cfor._
import cyclo.Cyclo
import net.alasc.finite.Grp
import scalin.immutable.{Vec, VecEngine}
import scalin.syntax.all._
import net.alasc.symdpoly.util.OrderedSet

/** Evaluated noncommutative polynomial. */
final class EvaluatedPoly[
  E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux
](val normalForm: M#Polynomial) extends EvaluatedPolyLike[E, M] { lhs: E#EvaluatedPolynomial =>

  def M: M = valueOf[M]
  def E: E = valueOf[E]

  def toPoly: EvaluatedPoly[E, M] = lhs

  override def toString: String = normalForm.string("L(", ")")
  override def hashCode: Int = normalForm.hashCode()
  override def equals(any: Any): Boolean = any match {
    case rhs: EvaluatedPoly[E, M] if (lhs.E eq rhs.E) => valueOf[M].polyEq.eqv(lhs.normalForm, rhs.normalForm)
    case _ => false
  }

  def nTerms: Int = normalForm.nTerms
  def monomial(i: Int): EvaluatedMono[E, M] = new EvaluatedMono[E, M](normalForm.monomial(i))
  def coeff(i: Int): Cyclo = normalForm.coeff(i)
  def coeff(mono: EvaluatedMono[E, M]): Cyclo = normalForm.coeff(mono.normalForm)

  def vecOverOrderedSet(orderedSet: OrderedSet[EvaluatedMono[E, M]])(implicit V: VecEngine[Cyclo]): Vec[Cyclo] =
    V.fromMutable(orderedSet.length, Cyclo.zero) { vec =>
      implicit def monoOrder: Order[EvaluatedMono[E, M]] = valueOf[E].evaluatedMonoOrder
      cforRange(0 until nTerms) { i =>
        val m = monomial(i)
        val c = coeff(i)
        val j = orderedSet.indexOf(m)
        if (j == -1) sys.error(s"Monomial $m not present in the moment set ${orderedSet}.") else { vec(j) := vec(j) + c }
      }
    }

  def invariantSubgroupOf(grp: Grp[E#Permutation]): Grp[E#Permutation] =
    symmetries.invariantSubgroupOf[E#EvaluatedMonomial, E#Permutation]((0 until nTerms).map(monomial), (x: EvaluatedMono[E, M]) => coeff(x), grp, M.cyclotomicOrder)(
      E.evaluatedMonoClassTag, E.evaluatedMonoOrder, E.evaluatedMonoPhased, E.permutationClassTag, E.permutationEq, E.permutationFaithfulPermutationActionBuilder, E.permutationGroup, E.evaluatedMonoPermutationAction)
}

object EvaluatedPoly {

  implicit def polyFromInt[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton:Witness.Aux
  ](i: Int): EvaluatedPoly[E, M] = valueOf[E].apply(valueOf[M].polyAssociativeAlgebra.fromInt(i))

  implicit def polyFromRational[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton:Witness.Aux
  ](r: Rational): EvaluatedPoly[E, M] = valueOf[E].apply(valueOf[M].polyAssociativeAlgebra.timesl(Cyclo.viewFromRational(r), valueOf[M].polyAssociativeAlgebra.one))

  implicit def polyFromCyclo[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton:Witness.Aux
  ](c: Cyclo): EvaluatedPoly[E, M] = valueOf[E].apply(valueOf[M].polyAssociativeAlgebra.timesl(c, valueOf[M].polyAssociativeAlgebra.one))

  //region Typeclasses

  implicit def equ[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ]: Eq[EvaluatedPoly[E, M]] = valueOf[E].evaluatedPolyEq

  implicit def vectorSpace[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ]: VectorSpace[EvaluatedPoly[E, M], Cyclo] = valueOf[E].evaluatedPolyVectorSpace

  //endregion

}
