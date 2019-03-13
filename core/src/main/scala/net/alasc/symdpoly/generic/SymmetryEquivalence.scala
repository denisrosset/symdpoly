package net.alasc.symdpoly
package generic

import shapeless.Witness
import spire.algebra.Action
import spire.syntax.action._

import net.alasc.finite.Grp

/** Equivalence under a group action. */
trait SymmetryEquivalence[M <: generic.MonoidDef with Singleton, G] extends Equivalence[M] {
  def grp: Grp[G]
  implicit def action: Action[M#Monomial, G]
}

object SymmetryEquivalence {

  def apply[M <: generic.MonoidDef with Singleton, G](grp0: Grp[G])(implicit action0: Action[M#Monomial, G], witnessM0: Witness.Aux[M]): SymmetryEquivalence[M, G] = new SymmetryEquivalence[M, G] {
    val witnessM: Witness.Aux[M] = witnessM0
    val action: Action[M#Monomial, G] = action0
    def grp: Grp[G] = grp0
    def apply(mono: M#Monomial): Set[M#Monomial] = grp0.iterator.map(g => mono <|+| g).toSet
    def groupInEvaluator(grp: Grp[M#Permutation]): Grp[M#Permutation] = Grp.trivial[M#Permutation]
  }

}
