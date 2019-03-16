package net.alasc.symdpoly.evaluation.internal

import shapeless.Witness

import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.Equivalence
import net.alasc.symdpoly.generic

/** Equivalence under the adjoint operation. */
final case class AdjointEquivalence[M <: generic.MonoidDef with Singleton]()(implicit val witnessM: Witness.Aux[M]) extends Equivalence[M] {
  def apply(mono: M#Monomial): Set[M#Monomial] = Set(mono, M.monoInvolution.adjoint(mono))
  def compatibleSubgroup(grp: Grp[M#Permutation]): Grp[M#Permutation] = grp
  override def isSelfAdjoint: Boolean = true
}
