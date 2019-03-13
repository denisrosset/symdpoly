package net.alasc.symdpoly.generic

import net.alasc.finite.Grp
import shapeless.Witness
import net.alasc.symdpoly.{generic, valueOf}

/** Equivalence under the adjoint operation. */
final class AdjointEquivalence[M <: generic.MonoidDef with Singleton](implicit val witnessM: Witness.Aux[M]) extends Equivalence[M] {
  def apply(mono: M#Monomial): Set[M#Monomial] = Set(mono, M.monoInvolution.adjoint(mono))
  def groupInEvaluator(grp: Grp[M#Permutation]): Grp[M#Permutation] = grp
}
