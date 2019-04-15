package net.alasc.symdpoly
package evaluation

import scala.collection.immutable.HashSet
import shapeless.Witness
import spire.syntax.action._
import spire.syntax.involution._
import spire.syntax.cfor._
import syntax.phased._
import net.alasc.finite.Grp
import net.alasc.symdpoly.generic.SingleMoment
import net.alasc.symdpoly.math.{GrpDecomposition, Phase}
import spire.syntax.std.seq._
import net.alasc.perms.default._
import net.alasc.symdpoly.free.MutableWord

import scala.annotation.tailrec

/** Equivalence under the adjoint operation. */
final case class ScratchTraceEvaluator[
  M <: freebased.MonoDef.Aux[F] with Singleton,
  F <: free.MonoDef.Aux[F] with Singleton
](real: Boolean, symmetryGroup: Grp[M#PermutationType])(implicit val witnessMono: Witness.Aux[M]) extends ScratchEvaluator[M, F] {

  protected lazy val unoptimized: Evaluator.Aux[M] = new TraceEvaluator[M, F](real, symmetryGroup)

  implicit def witnessF: Witness.Aux[F] = valueOf[M].witnessFree

  protected def populatePadAndCheckForZero(pad: FreeScratchPad[F], mono: M#MonoType): Boolean = {
    pad.resetWithCopyOf(mono.data)
    if (real) {
      pad.scratch(1).setToContentOf(mono.data)
      pad.scratch(1).inPlaceAdjoint()
      M.inPlaceNormalForm(pad.scratch(1))
      if (pad.registerAndTestForZero(2)) return true
    }
    @tailrec def cycleAndTestForZero(start: Int): Boolean = {
      var ind = pad.n
      val thisEnd = pad.n
      cforRange(start until thisEnd) { i =>
        pad.scratch(ind).setToContentOf(pad.scratch(i))
        pad.scratch(ind).rotateRight()
        M.inPlaceNormalForm(pad.scratch(ind))
        ind += 1
      }
      if (pad.registerAndTestForZero(ind)) return true
      if (pad.n > thisEnd) cycleAndTestForZero(thisEnd) else false
    }
    if (mono.data.length > 1) cycleAndTestForZero(0) else false
  }

  protected def buildWithSymmetryGroup(newSymmetryGroup: Grp[M#PermutationType]): Evaluator.Aux[M] =
    new ScratchTraceEvaluator[M, F](real, newSymmetryGroup)

  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = grp

  def isReal: Boolean = real

}
