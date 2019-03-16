package net.alasc.symdpoly.evaluation

import shapeless.Witness

import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.internal.ComposedEquivalence
import net.alasc.symdpoly.{generic, valueOf}

/** An equivalence relation on monomials. */
trait Equivalence[M <: generic.MonoidDef with Singleton] { self =>

  implicit def witnessM: Witness.Aux[M]
  def M: M = valueOf[M]

  /** Returns the set of monomials equivalent to the given monomial. */
  def apply(mono: M#Monomial): Set[M#Monomial]

  /** Returns whether the given group is compatible with this equivalence relation. */
  def isCompatibleGroup(grp: Grp[M#Permutation]): Boolean = grp === compatibleSubgroup(grp)

  /** Returns the group of permutations that is compatible with the evaluator,
    *
    * A group of permutations is deemed compatible if, for any monomials m1 and m2 of type M#Monomial,
    * we have m1 ~ m2 if and only if (m1 <|+| g) ~ (m2 <|+| g) for any element g of the group,
    * where ~ is described by this equivalence relation.
    */
  def compatibleSubgroup(grp: Grp[M#Permutation]): Grp[M#Permutation]

  /** Returns whether m.adjoint is equivalent to m for all m of type M#Monomial. */
  def isSelfAdjoint: Boolean

  def andThen(next: Equivalence[M]): Equivalence[M] = {
    val before = self match {
      case ComposedEquivalence(seq) => seq
      case e => Seq(e)
    }
    val after = next match {
      case ComposedEquivalence(seq) => seq
      case e => Seq(e)
    }
    ComposedEquivalence(before ++ after)
  }

}
