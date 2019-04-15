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
import net.alasc.symdpoly.evaluation.parts.{OpPartition, PartiallyCommutative}

/** Equivalence under the adjoint operation. */
final class ScratchPPTEvaluator[
  M <: freebased.MonoDef.Aux[F] with Singleton,
  F <: free.MonoDef.Aux[F] with Singleton
](val partition: OpPartition[F], val symmetryGroup: Grp[M#PermutationType])(implicit val witnessMono: Witness.Aux[M]) extends ScratchEvaluator[M, F] {

  protected lazy val unoptimized: Evaluator.Aux[M] = new PPTEvaluator[M, F](partition, symmetryGroup)

  implicit def witnessF: Witness.Aux[F] = valueOf[M].witnessFree

  def F: F = valueOf[F]
  /** Performs the in place partial transpose of the group of operators selected by the given predicate.
    *
    * Returns whether any changes have been made.
    */
  @tailrec protected def inPlace(word: free.MutableWord[F], l: Int, r: Int, block: Int, changed: Boolean = false): Boolean =
        if (l > r) changed
        else if (l == r) {
          if (partition.underlying.blockFor(word.indices(l)) == block) {
            val la = F.indexAdjoint(word.indices(l))
            if (la != word.indices(l)) {
              word.indices(l)
              true
            } else changed
          } else changed
        } else if (partition.underlying.blockFor(word.indices(l)) == block && partition.underlying.blockFor(word.indices(r)) == block) {
          val la = F.indexAdjoint(word.indices(l))
          if (la != word.indices(r)) {
            val ra = F.indexAdjoint(word.indices(r))
            word.indices(l) = ra
            word.indices(r) = la
            inPlace(word, l + 1, r - 1, block, true)
          }
          else inPlace(word, l + 1, r - 1, block, changed)
        } else if (partition.underlying.blockFor(word.indices(l)) == block) inPlace(word, l, r - 1, block, changed)
        else if (partition.underlying.blockFor(word.indices(r)) == block) inPlace(word, l + 1, r, block, changed)
        else inPlace(word, l + 1, r - 1, block, changed)

  /** Populates the given pad with equivalences of the given monomial (not including symmetry equivalences). */
  protected def populatePadAndCheckForZero(pad: FreeScratchPad[F], mono: Mono#MonoType): Boolean = {
    pad.resetWithCopyOf(mono.data)
    if (mono.data.length > 1) {
      cforRange(0 until partition.underlying.nBlocks) { block =>
        val thisEnd = pad.n
        var ind = pad.n
        cforRange(0 until thisEnd) { i =>
          pad.scratch(ind).setToContentOf(pad.scratch(i))
          if (inPlace(pad.scratch(ind), 0, pad.scratch(ind).length - 1, block)) {
            M.inPlaceNormalForm(pad.scratch(ind))
            ind += 1
          }
        }
        if (pad.registerAndTestForZero(ind)) return true
      }
    }
    false
  }

  protected def buildWithSymmetryGroup(newSymmetryGroup: Grp[M#PermutationType]): Evaluator.Aux[M] =
    new PPTEvaluator[M, F](partition, newSymmetryGroup)

  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = grp

  def isReal: Boolean = true

}

object ScratchPPTEvaluator {

  def apply[
    M <: quotient.MonoDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoDef.Aux[F] with Singleton
  ](partition: OpPartition[F], symmetryGroup: Grp[M#PermutationType]): ScratchPPTEvaluator[M, F] =
    PartiallyCommutative[F](valueOf[M]: M).partition.fold(err => sys.error(s"Invalid partially commutative monoid: $err"), parts => {
      require(partition >= parts, s"The PPT partition $partition should be equal or coarser than the commutative partition $parts")
      new ScratchPPTEvaluator[M, F](partition, symmetryGroup)
    })

}