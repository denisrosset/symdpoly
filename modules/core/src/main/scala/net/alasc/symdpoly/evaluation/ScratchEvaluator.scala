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
abstract class ScratchEvaluator[
  M <: freebased.MonoDef.Aux[F] with Singleton,
  F <: free.MonoDef.Aux[F] with Singleton
] extends Evaluator {

  implicit def witnessF: Witness.Aux[F]

  protected def unoptimized: Evaluator.Aux[M]

  type Mono = M

  /** Populates the given pad with equivalences of the given monomial (not including symmetry equivalences).
    * If it returns true, the monomial is equivalent to zero.
    */
  protected def populatePadAndCheckForZero(pad: FreeScratchPad[F], mono: Mono#MonoType): Boolean

  def apply(mono: Mono#MonoType): SingleMomentType =
    if (!Settings.optimize) new SingleMoment[this.type, M](unoptimized(mono).normalForm)
    else if (mono.isZero) zero
    else {
      val pad = FreeScratchPad[F]
      if (populatePadAndCheckForZero(pad, mono)) {
        FreeScratchPad.release(pad)
        return this.zero
      }
      @tailrec def recAndTestForZero(pos: Int): Boolean =
        if (pos >= pad.n) false else {
          var newPos = pad.n
          cforRange(0 until symmetryGroup.nGenerators) { genI =>
            val g = symmetryGroup.generator(genI).genPerm
            pad.scratch(newPos).setToContentOf(pad.scratch(pos))
            pad.scratch(newPos).setPhase(Phase.fromEncoding(pad.phaseArray(pos)))
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
      var imin = 0
      cforRange(1 until pad.n) { i =>
        if (pad.scratch(i) < pad.scratch(imin))
          imin = i
      }
      val resWord = pad.scratch(imin).mutableCopy().setPhase(Phase.fromEncoding(pad.phaseArray(imin))).setImmutable()
      val res = fromNormalForm(new freebased.Mono[M, F](resWord))
      FreeScratchPad.release(pad)
      res
    }

}
