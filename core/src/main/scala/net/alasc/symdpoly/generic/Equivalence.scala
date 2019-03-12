package net.alasc.symdpoly
package generic

import net.alasc.finite.Grp

/** An equivalence relation on monomials. */
abstract class Equivalence[M <: generic.MonoidDef with Singleton] {

  /** Returns the set of monomials equivalent to the given monomial. */
  def apply(mono: M#Monomial): Set[M#Monomial]

  /** Returns the subgroup of a group of permutations on the monoid that is compatible with the evaluator. */
  def groupInEvaluator(grp: Grp[M#Permutation]): Grp[M#Permutation]

  /** Translates a group on the monoid onto a group on the evaluated objects, without checking compatibility. */
  def groupInEvaluatorNC(grp: Grp[M#Permutation]): Grp[M#Permutation]

}
