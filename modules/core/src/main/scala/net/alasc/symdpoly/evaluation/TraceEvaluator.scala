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
final case class TraceEvaluator[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](real: Boolean, symmetryGroup: Grp[M#PermutationType])(implicit val witnessMono: Witness.Aux[M]) extends Evaluator {

  implicit def witnessF: Witness.Aux[F] = valueOf[M].witnessFree

  type Mono = M

  def allCyclicPermutations(mono: M#MonoType): Set[M#MonoType] = {
    val normalForm = mono.normalForm
    if (normalForm.length <= 1) Set(mono) else {
      var rewritten = false
      val l = mono.data.length
      val res = Seq.tabulate(l) { i =>
        val w = mono.data.mutableCopy
        cforRange(0 until l) { j =>
          w(j) = mono.data((i + j) % l)
        }
        rewritten |= M.inPlaceNormalForm(w)
        new freebased.Mono[M, F](w.setImmutable())
      }.toSet
      if (rewritten) res.flatMap(allCyclicPermutations) else res
    }
  }

  def apply(mono: M#MonoType): SingleMomentType = {
    val start: Set[M#MonoType] = if (real) HashSet(mono, mono.adjoint) else HashSet(mono)
    val afterCyclic: Set[M#MonoType] = start.flatMap(allCyclicPermutations)
    val candidates: Set[M#MonoType] = symmetrizeMonoSet[M](afterCyclic, symmetryGroup)
    fromNormalForm(findCanonicalInSet[M](candidates))
  }

  protected def buildWithSymmetryGroup(newSymmetryGroup: Grp[M#PermutationType]): Evaluator.Aux[M] =
    new EigenvalueEvaluator[M](real, newSymmetryGroup)

  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = grp

  def isReal: Boolean = real

}
