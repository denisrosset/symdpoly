package net.alasc.symdpoly

import net.alasc.finite.Grp
import shapeless.Witness
import spire.syntax.action._
import spire.syntax.involution._
import spire.syntax.cfor._
import spire.syntax.std.seq._

import syntax.phased._
import net.alasc.perms.default._
import net.alasc.symdpoly.math.GrpDecomposition

package object evaluation {

  /** Computes the orbit of the given set of monomials under the given symmetry group. */
  def symmetrizeMonoSet[
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](start: Set[M#MonoType], symmetryGroup: Grp[M#PermutationType]): Set[M#MonoType] = {
    def M: M = valueOf[M]
    def applyTransversal(elements: Set[M#MonoType], transversal: Vector[M#PermutationType]): Set[M#MonoType] =
      elements.flatMap(m => transversal.map(g => m <|+| g))
    if (Settings.optimize)
      symmetryGroup.decomposition.transversals.foldLeft(start) { case (set, transversal) => applyTransversal(set, transversal) }
    else start.flatMap(m => symmetryGroup.iterator.map(g => m <|+| g))
  }

  /** Find the canonical representative in a set of monomial, or returns zero if the set contains
    * two or more monomials that differ only by a phase.
    */
  def findCanonicalInSet[M <: generic.MonoidDef with Singleton: Witness.Aux](candidates: Set[M#MonoType]): M#MonoType = {
    def M: M = valueOf[M]
    val canonical = candidates.map(_.phaseCanonical)
    if (canonical.size != candidates.size) M.monoMultiplicativeBinoid.zero else candidates.qmin(M.monoOrder)
  }

}
