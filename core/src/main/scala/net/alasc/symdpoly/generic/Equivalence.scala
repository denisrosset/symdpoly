package net.alasc.symdpoly
package generic

import shapeless.Witness

import net.alasc.finite.Grp

/** An equivalence relation on monomials. */
trait Equivalence[M <: generic.MonoidDef with Singleton] {

  implicit def witnessM: Witness.Aux[M]
  def M: M = valueOf[M]

  /** Returns the set of monomials equivalent to the given monomial. */
  def apply(mono: M#Monomial): Set[M#Monomial]

  /** Returns the subgroup of a group of permutations on the monoid that is compatible with the evaluator. */
  def groupInEvaluator(grp: Grp[M#Permutation]): Grp[M#Permutation]

}
