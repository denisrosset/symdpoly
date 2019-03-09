package net.alasc.symdpoly
package evaluation

import shapeless.Witness
import spire.algebra.{Eq, Order, VectorSpace}
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.vectorSpace._

import scalin.syntax.all._
import cyclo.Cyclo

import net.alasc.symdpoly.util.OrderedSet
import scalin.immutable.{Vec, VecEngine}

import net.alasc.symdpoly
import net.alasc.symdpoly.{generic, valueOf}

trait EvaluatedPolyLike[
  E <: Evaluator[M] with Singleton,
  M <: generic.MonoidDef with Singleton
] { lhs =>

  def toPoly: EvaluatedPoly[E, M]

  def <=!(rhs: EvaluatedPolyLike[E, M])(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] =
    EvaluatedConstraint(lhs.toPoly, ComparisonOp.LE, rhs.toPoly)

  def <=!(rhs: Int)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] = lhs <=! valueOf[E].constant(rhs)
  def <=!(rhs: Rational)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] = lhs <=! valueOf[E].constant(rhs)
  def <=!(rhs: Cyclo)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] = lhs <=! valueOf[E].constant(rhs)

  def >=!(rhs: EvaluatedPolyLike[E, M])(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] =
    EvaluatedConstraint(lhs.toPoly, ComparisonOp.GE, rhs.toPoly)

  def >=!(rhs: Int)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] = lhs >=! valueOf[E].constant(rhs)
  def >=!(rhs: Rational)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] = lhs >=! valueOf[E].constant(rhs)
  def >=!(rhs: Cyclo)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] = lhs >=! valueOf[E].constant(rhs)

  def =!(rhs: EvaluatedPolyLike[E, M])(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] =
    EvaluatedConstraint(lhs.toPoly, ComparisonOp.EQ, rhs.toPoly)

  def =!(rhs: Int)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] = lhs =! valueOf[E].constant(rhs)
  def =!(rhs: Rational)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] = lhs =! valueOf[E].constant(rhs)
  def =!(rhs: Cyclo)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): EvaluatedConstraint[E, M] = lhs =! valueOf[E].constant(rhs)

  def maximize(implicit E: Witness.Aux[E], M: Witness.Aux[M]): Optimization[E, M] = new Optimization[E, M](Direction.Maximize, lhs.toPoly)

  def minimize(implicit E: Witness.Aux[E], M: Witness.Aux[M]): Optimization[E, M] = new Optimization[E, M](Direction.Minimize, lhs.toPoly)

}

/** Evaluated noncommutative polynomial. */
final class EvaluatedPoly[
  E <: Evaluator[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux
](val normalForm: M#Polynomial) extends EvaluatedPolyLike[E, M] { lhs =>

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

}

object EvaluatedPoly {

  implicit def polyFromInt[
    E <: Evaluator[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton:Witness.Aux
  ](i: Int): EvaluatedPoly[E, M] = valueOf[E].apply(valueOf[M].polyAssociativeAlgebra.fromInt(i))

  implicit def polyFromRational[
    E <: Evaluator[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton:Witness.Aux
  ](r: Rational): EvaluatedPoly[E, M] = valueOf[E].apply(valueOf[M].polyAssociativeAlgebra.timesl(Cyclo.viewFromRational(r), valueOf[M].polyAssociativeAlgebra.one))

  implicit def polyFromCyclo[
    E <: Evaluator[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton:Witness.Aux
  ](c: Cyclo): EvaluatedPoly[E, M] = valueOf[E].apply(valueOf[M].polyAssociativeAlgebra.timesl(c, valueOf[M].polyAssociativeAlgebra.one))

  //region Typeclasses

  implicit def equ[
    E <: Evaluator[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ]: Eq[EvaluatedPoly[E, M]] = valueOf[E].evaluatedPolyEq

  implicit def vectorSpace[
    E <: Evaluator[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ]: VectorSpace[EvaluatedPoly[E, M], Cyclo] = valueOf[E].evaluatedPolyVectorSpace

  //endregion

}
