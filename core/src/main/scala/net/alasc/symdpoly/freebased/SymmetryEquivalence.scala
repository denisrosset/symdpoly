package net.alasc.symdpoly
package freebased

import shapeless.Witness

import net.alasc.finite.Grp
import spire.syntax.action._

/** Equivalence under a group action. */
final class SymmetryEquivalence[
  M <: freebased.MonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](val grp: Grp[freebased.Permutation[M, F]]) extends Equivalence[M, F] {

  def apply(mono: M#Monomial): Set[M#Monomial] = grp.iterator.map(g => valueOf[M].permutationMonoAction.actr(mono, g)).toSet

}
