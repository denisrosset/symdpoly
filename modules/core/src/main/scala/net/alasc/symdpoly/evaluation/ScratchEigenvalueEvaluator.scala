package net.alasc.symdpoly
package evaluation

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

import shapeless.Witness
import spire.syntax.action._
import spire.syntax.involution._
import spire.syntax.cfor._
import spire.syntax.order._

import syntax.phased._
import net.alasc.finite.Grp
import net.alasc.symdpoly.generic.SingleMoment
import net.alasc.symdpoly.math.{GrpDecomposition, Phase}
import spire.syntax.std.seq._

import net.alasc.perms.default._

/** Equivalence under the adjoint operation. */
final case class ScratchEigenvalueEvaluator[
  M <: freebased.MonoDef.Aux[F] with Singleton,
  F <: free.MonoDef.Aux[F] with Singleton
](real: Boolean, symmetryGroup: Grp[M#PermutationType])(implicit val witnessMono: Witness.Aux[M]) extends ScratchEvaluator[M, F] {

  protected lazy val unoptimized: Evaluator.Aux[M] = new EigenvalueEvaluator[M](real, symmetryGroup)

  implicit def witnessF: Witness.Aux[F] = valueOf[M].witnessFree

  /** Populates the given pad with equivalences of the given monomial (not including symmetry equivalences). */
  protected def populatePadAndCheckForZero(pad: FreeScratchPad[F], mono: Mono#MonoType): Boolean = {
    pad.resetWithCopyOf(mono.data)
    if (real) {
      pad.scratch(1).setToContentOf(mono.data)
      pad.scratch(1).inPlaceAdjoint()
      M.inPlaceNormalForm(pad.scratch(1))
      pad.registerAndTestForZero(2)
    } else false
  }

  protected def buildWithSymmetryGroup(newSymmetryGroup: Grp[M#PermutationType]): Evaluator.Aux[M] =
    new ScratchEigenvalueEvaluator[M, F](real, newSymmetryGroup)

  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = grp

  def isReal: Boolean = real

}
