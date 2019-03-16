package net.alasc.symdpoly
package evaluation
package internal

import shapeless.Witness

import net.alasc.finite.Grp

case class ComposedEquivalence[
  M <: generic.MonoidDef with Singleton
](equivalences: Seq[Equivalence[M]])(implicit val witnessM: Witness.Aux[M]) extends Equivalence[M] {
  def apply(mono: M#Monomial): Set[M#Monomial] =
    equivalences.foldLeft(Set(mono)) {
      case (res, equiv) => res.flatMap(m => equiv.apply(m))
    }
  def compatibleSubgroup(grp: Grp[M#Permutation]): Grp[M#Permutation] = equivalences.foldLeft(grp) {
    case (res, e) => e.compatibleSubgroup(res)
  }
  def isSelfAdjoint: Boolean = equivalences.nonEmpty && equivalences.last.isSelfAdjoint
}
