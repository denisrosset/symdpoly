package net.alasc.symdpoly
package generic

import scala.annotation.tailrec
import scala.reflect.ClassTag

import shapeless.Witness
import spire.algebra.Action
import spire.syntax.action._

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.math.GrpDecomposition
import net.alasc.perms.default._

/** Equivalence under a group action. */
trait SymmetryEquivalence[M <: generic.MonoidDef with Singleton, G] extends Equivalence[M] {
  def grp: Grp[G]
  implicit def action: Action[M#Monomial, G]
}

object SymmetryEquivalence {

  def apply[M <: generic.MonoidDef with Singleton, G:ClassTag:FaithfulPermutationActionBuilder](grp0: Grp[G])(implicit action0: Action[M#Monomial, G], witnessM0: Witness.Aux[M]): SymmetryEquivalence[M, G] = new SymmetryEquivalence[M, G] {
    import grp0.{equ, group}
    val witnessM: Witness.Aux[M] = witnessM0
    val action: Action[M#Monomial, G] = action0
    def grp: Grp[G] = grp0
    val grpDecomposition: GrpDecomposition[G] = GrpDecomposition(grp)
    def apply(mono: M#Monomial): Set[M#Monomial] = {
      @tailrec def rec(set: Set[M#Monomial], trv: List[Seq[G]]): Set[M#Monomial] = trv match {
        case Nil => set
        case hd :: tl => rec(set.flatMap(mono => hd.map(g => mono <|+| g)), tl)
      }
      rec(Set(mono), grpDecomposition.transversals)
    }
/*      grpDecomposition.transversals.foldLeft()
      grp0.iterator.map(g => mono <|+| g).toSet*/
    def groupInEvaluator(grp: Grp[M#Permutation]): Grp[M#Permutation] = Grp.trivial[M#Permutation]
  }

}
