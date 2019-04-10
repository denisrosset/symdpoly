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
final case class FreeBasedEigenvalueEvaluator[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](real: Boolean, symmetryGroup: Grp[M#PermutationType])(implicit val witnessMono: Witness.Aux[M]) extends Evaluator {

  println("Created freebased")
  implicit def witnessF: Witness.Aux[F] = valueOf[M].witnessFree

  type Mono = M

  def apply(mono: Mono#MonoType): SingleMomentType =
  if (mono.isZero) zero else {
    val pad = FreeScratchPad[F]
    pad.resetWithCopyOf(mono.data)
    if (real) {
      pad.scratch(1).setToContentOf(mono.data)
      pad.scratch(1).inPlaceAdjoint()
      M.inPlaceNormalForm(pad.scratch(1))
      if (pad.registerAndTestForZero(2)) {
        FreeScratchPad.release(pad)
        return this.zero
      }
    }

    @tailrec def recAndTestForZero(pos: Int): Boolean =
      if (pos == pad.n) false else {
        var newPos = pad.n
        cforRange(0 until symmetryGroup.nGenerators) { genI =>
          val g = symmetryGroup.generator(genI).genPerm
          pad.scratch(newPos).setToContentOf(pad.scratch(pos))
          pad.scratch(newPos).inPlaceGenPermAction(g)
          M.inPlaceNormalForm(pad.scratch(newPos))
          newPos += 1
        }
        if (pad.registerAndTestForZero(newPos)) true else recAndTestForZero(pos + 1)
      }

    if (recAndTestForZero(0)) {
      FreeScratchPad.release(pad)
      return this.zero
    }
    var elmin = pad.scratch(0)
    cforRange(1 until pad.n) { i =>
      if (pad.scratch(i) < elmin)
        elmin = pad.scratch(i)
    }
    val res = new SingleMoment[this.type, M](new freebased.Mono[M, F](elmin.immutableCopy))
    FreeScratchPad.release(pad)
    res
  }

  protected def buildWithSymmetryGroup(newSymmetryGroup: Grp[M#PermutationType]): Evaluator.Aux[M] =
    new FreeBasedEigenvalueEvaluator[M, F](real, newSymmetryGroup)

  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = grp

  def isReal: Boolean = real

}
