package net.alasc.symdpoly
package evaluation

import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.generic.FreeBasedMonoidDef
import net.alasc.symdpoly.math.GenPerm
import shapeless.Witness
import spire.algebra.{Action, Order}

import net.alasc.finite.Grp

final class EvaluatedMono[
  E <: Evaluator[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux,
  G <: Grp[GenPerm] with Singleton:Witness.Aux
](val normalForm: M#Monomial) { lhs =>

  def E: E = valueOf[E]

  override def toString: String = "L(" + normalForm.toString + ")"
  override def hashCode: Int = normalForm.hashCode()
  override def equals(any: Any): Boolean = any match {
    case rhs: EvaluatedMono[E, M, G] if lhs.E eq rhs.E => E.evaluatedMonoOrder.eqv(lhs, rhs)
    case _ => false
  }

  def phaseOffset: Phase = valueOf[M].monoPhased.phaseOffset(normalForm)
  def phaseCanonical: EvaluatedMono[E, M, G] =
    new EvaluatedMono[E, M, G](valueOf[M].monoPhased.phaseCanonical(normalForm))
  def *(phase: Phase): EvaluatedMono[E, M, G] =
    new EvaluatedMono[E, M, G](valueOf[M].monoPhased.gtimesr(normalForm, phase))

}

object EvaluatedMono {

  implicit def order[
    E <: Evaluator[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton,
    G <: Grp[GenPerm] with Singleton
  ]: Order[EvaluatedMono[E, M, G]] = valueOf[E].evaluatedMonoOrder[G]

  implicit def phased[
    E <: FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
    M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton,
    G <: Grp[GenPerm] with Singleton
  ]: Phased[EvaluatedMono[E, M, G]] = valueOf[E].evaluatedMonoPhased[G]

  implicit def genPermAction[
    E <: FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
    M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton,
    G <: Grp[GenPerm] with Singleton:Witness.Aux
  ]: Action[EvaluatedMono[E, M, G], GenPerm] =
    new EvaluatedMonoGenPermAction[E, M, F, G]

}

final class EvaluatedMonoPhased[
E <: FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
F <: free.MonoidDef.Aux[F] with Singleton,
G <: Grp[GenPerm] with Singleton
] extends Phased[EvaluatedMono[E, M, G]] {
  def phaseOffset(a: EvaluatedMono[E, M, G]): Phase = a.phaseOffset
  def phaseCanonical(a: EvaluatedMono[E, M, G]): EvaluatedMono[E, M, G] = a.phaseCanonical
  def gtimesl(g: Phase, p: EvaluatedMono[E, M, G]): EvaluatedMono[E, M, G] = gtimesr(p, g.reciprocal)
  def gtimesr(p: EvaluatedMono[E, M, G], g: Phase): EvaluatedMono[E, M, G] = p * g
}

final class EvaluatedMonoGenPermAction[
  E <: FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton,
  G <: Grp[GenPerm] with Singleton:Witness.Aux
](implicit actionM: Action[Mono[M, F], GenPerm]) extends Action[EvaluatedMono[E, M, G], GenPerm]  {
  def actl(g: GenPerm, m: EvaluatedMono[E, M, G]): EvaluatedMono[E, M, G] = actr(m, g.inverse)
  def actr(m: EvaluatedMono[E, M, G], g: GenPerm): EvaluatedMono[E, M, G] = valueOf[E].apply(actionM.actr(m.normalForm, g), valueOf[G])
}
