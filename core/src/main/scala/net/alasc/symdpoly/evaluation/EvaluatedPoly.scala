package net.alasc.symdpoly
package evaluation

import shapeless.Witness
import spire.algebra.{Eq, Order, VectorSpace}
import spire.syntax.cfor._
import scalin.syntax.all._
import cyclo.Cyclo
import scalin.immutable.{Vec, VecEngine}

import net.alasc.symdpoly.{OrderedSet, generic, valueOf}

final class EvaluatedPoly[E <: Evaluator[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton:Witness.Aux](val normalForm: M#Polynomial) { lhs =>

  def E: E = valueOf[E]

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

  def maximize: Maximization[E, M] = new Maximization[E, M](lhs)
}

object EvaluatedPoly {

  implicit def equ[E <: Evaluator[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton]: Eq[EvaluatedPoly[E, M]] = valueOf[E].evaluatedPolyEq

  implicit def vectorSpace[E <: Evaluator[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton]: VectorSpace[EvaluatedPoly[E, M], Cyclo] = valueOf[E].evaluatedPolyVectorSpace

}
