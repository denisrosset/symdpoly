package net.alasc.symdpoly
package evaluation

import shapeless.Witness
import spire.algebra.{Action, Order}

import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.generic.FreeBasedPermutation

/** Evaluated monomial, i.e. equivalence class under some evaluator. */
final class EvaluatedMono[
  E <: Evaluator[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux
](val normalForm: M#Monomial) extends EvaluatedPolyLike[E, M] { lhs =>
  def E: E = valueOf[E]
  def M: M = valueOf[M]

  def toPoly: EvaluatedPoly[E, M] = E.apply(M.monomialToPolynomial(normalForm))

  override def toString: String = "L(" + normalForm.toString + ")"
  override def hashCode: Int = normalForm.hashCode()
  override def equals(any: Any): Boolean = any match {
    case rhs: EvaluatedMono[E, M] if lhs.E eq rhs.E => E.evaluatedMonoOrder.eqv(lhs, rhs)
    case _ => false
  }
  def isZero: Boolean = M.monoMultiplicativeBinoid.isZero(normalForm)(M.monoOrder)

}

object EvaluatedMono {

  //region Typeclasses

  implicit def order[
    E <: Evaluator[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ]: Order[EvaluatedMono[E, M]] = valueOf[E].evaluatedMonoOrder

  implicit def phased[
    E <: Evaluator[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ]: Phased[EvaluatedMono[E, M]] = valueOf[E].evaluatedMonoPhased

  implicit def evaluatedMonoAction[
    E <: Evaluator[M] with Singleton: Witness.Aux,
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ]: Action[EvaluatedMono[E, M], FreeBasedPermutation[M, F]] = (valueOf[E]: E).evaluatedMonoPermutationAction

  //endregion
}
