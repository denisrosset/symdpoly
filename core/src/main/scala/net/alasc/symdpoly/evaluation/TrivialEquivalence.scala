package net.alasc.symdpoly
package evaluation

import net.alasc.finite.Grp
import shapeless.Witness

/** Equivalence under the adjoint operation. */
final case class TrivialEquivalence[M <: generic.MonoidDef with Singleton]()(implicit val witnessM: Witness.Aux[M]) extends Equivalence[M] {
  def apply(mono: M#Monomial): Set[M#Monomial] = Set(mono)
  def compatibleSubgroup(grp: Grp[M#Permutation]): Grp[M#Permutation] = grp
  override def isSelfAdjoint: Boolean = true
}
