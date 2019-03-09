package net.alasc.symdpoly
package generic

import spire.algebra.Action
import spire.syntax.action._
import net.alasc.finite.Grp

/** Equivalence under a group action. */
final class SymmetryEquivalence[M <: generic.MonoidDef with Singleton, G](val grp: Grp[G])(implicit val action: Action[M#Monomial, G]) extends Equivalence[M] {
  def apply(mono: M#Monomial): Set[M#Monomial] = grp.iterator.map(g => mono <|+| g).toSet
}
