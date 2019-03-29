package net.alasc.symdpoly.evaluation

import net.alasc.finite.Grp
import net.alasc.symdpoly.generic
import shapeless.Witness

/** Equivalence under the adjoint operation. */
final case class AdjointEquivalence[M <: generic.MonoidDef with Singleton]()(implicit val witnessM: Witness.Aux[M]) extends Equivalence[M] {
  def apply(mono: M#MonoType): Set[M#MonoType] = Set(mono, M.monoInvolution.adjoint(mono))
  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = grp
  override def isSelfAdjoint: Boolean = true
}
