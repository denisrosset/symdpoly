package net.alasc.symdpoly.evaluation

import shapeless.Witness
import spire.algebra.Order

import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.{generic, valueOf}

final class EvaluatedMono2[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton:Witness.Aux](val normalForm: M#Monomial) { lhs =>
  def E: E = valueOf[E]
  def M: M = valueOf[M]

  override def toString: String = "L(" + normalForm.toString + ")"
  override def hashCode: Int = normalForm.hashCode()
  override def equals(any: Any): Boolean = any match {
    case rhs: EvaluatedMono2[E, M] if lhs.E eq rhs.E => E.evaluatedMonoOrder.eqv(lhs, rhs)
    case _ => false
  }
  def isZero: Boolean = M.monoMultiplicativeBinoid.isZero(normalForm)(M.monoOrder)

}

object EvaluatedMono2 {
  implicit def order[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton]: Order[EvaluatedMono2[E, M]] = valueOf[E].evaluatedMonoOrder
  implicit def phased[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton]: Phased[EvaluatedMono2[E, M]] = valueOf[E].evaluatedMonoPhased
}
