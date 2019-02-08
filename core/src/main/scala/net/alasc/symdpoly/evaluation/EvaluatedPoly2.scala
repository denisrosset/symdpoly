package net.alasc.symdpoly
package evaluation

import shapeless.Witness
import spire.algebra.{Eq, Order, VectorSpace}
import spire.syntax.cfor._
import scalin.syntax.all._
import cyclo.Cyclo
import scalin.immutable.{Vec, VecEngine}

import net.alasc.symdpoly.{OrderedSet, generic, valueOf}

final class EvaluatedPoly2[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton:Witness.Aux](val normalForm: M#Polynomial) { lhs =>

  def E: E = valueOf[E]

  override def toString: String = normalForm.string("L(", ")")
  override def hashCode: Int = normalForm.hashCode()
  override def equals(any: Any): Boolean = any match {
    case rhs: EvaluatedPoly2[E, M] if (lhs.E eq rhs.E) => valueOf[M].polyEq.eqv(lhs.normalForm, rhs.normalForm)
    case _ => false
  }

  def nTerms: Int = normalForm.nTerms
  def monomial(i: Int): EvaluatedMono2[E, M] = new EvaluatedMono2[E, M](normalForm.monomial(i))
  def coeff(i: Int): Cyclo = normalForm.coeff(i)

  def vecOverOrderedSet(orderedSet: OrderedSet[EvaluatedMono2[E, M]])(implicit V: VecEngine[Cyclo]): Vec[Cyclo] =
    V.fromMutable(orderedSet.length, Cyclo.zero) { vec =>
      implicit def monoOrder: Order[EvaluatedMono2[E, M]] = valueOf[E].evaluatedMonoOrder
      cforRange(0 until nTerms) { i =>
        val m = monomial(i)
        val c = coeff(i)
        val j = orderedSet.indexOf(m)
        if (j == -1) sys.error(s"Monomial $m not present in the moment set ${orderedSet}.") else { vec(j) := vec(j) + c }
      }
    }

  def maximize: Maximization2[E, M] = new Maximization2[E, M](lhs)
}

object EvaluatedPoly2 {

  implicit def equ[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton]: Eq[EvaluatedPoly2[E, M]] = valueOf[E].evaluatedPolyEq

  implicit def vectorSpace[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton]: VectorSpace[EvaluatedPoly2[E, M], Cyclo] = valueOf[E].evaluatedPolyVectorSpace

}