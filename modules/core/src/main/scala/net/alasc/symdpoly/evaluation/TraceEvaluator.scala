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

/** Equivalence under the adjoint operation. */
final case class TraceEvaluator[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F]
](real: Boolean, symmetryGroup: Grp[M#PermutationType])(implicit val witnessMono: Witness.Aux[M]) extends Evaluator {

  implicit def witnessF: Witness.Aux[F] = valueOf[M].witnessFree

  type Mono = M

  lazy val symmetryGroupDecomposition: GrpDecomposition[M#PermutationType] = GrpDecomposition(symmetryGroup)

  protected def applyTransversal(elements: Set[M#MonoType],
                                 transversal: Vector[M#PermutationType]): Set[M#MonoType] =
    elements.flatMap(m => transversal.map(g => m <|+| g))

  def allCyclicPermutations(mono: M#MonoType): Set[M#MonoType] =
    if (mono.data.length <= 1) Set(mono) else {
      val l = mono.data.length
      Seq.tabulate(l) { i =>
        val w = mono.data.mutableCopy
        cforRange(0 until l) { j =>
          w(j) = mono.data((i + j) % l)
        }
        new freebased.Mono[M, F](w.setImmutable())
      }.toSet
  }

  def apply(mono: M#MonoType): SingleMomentType = {
    val start: Set[M#MonoType] = if (real) HashSet(mono, mono.adjoint) else HashSet(mono)
    val afterCyclic: Set[M#MonoType] = start.flatMap(allCyclicPermutations)
    val candidates: Set[M#MonoType] =
      if (Settings.optimize) symmetryGroupDecomposition.transversals.foldLeft(start) { case (set, transversal) => applyTransversal(set, transversal) }
      else start.flatMap(m => symmetryGroup.iterator.map(g => m <|+| g))
    val canonical = candidates.map(_.phaseCanonical)
    if (canonical.size != candidates.size)
      new SingleMoment[this.type, M](M.monoMultiplicativeBinoid.zero)
    else
      new SingleMoment[this.type, M](candidates.qmin(M.monoOrder))
  }

  protected def buildWithSymmetryGroup(newSymmetryGroup: Grp[M#PermutationType]): Evaluator.Aux[M] =
    new EigenvalueEvaluator[M](real, newSymmetryGroup)

  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = grp

  def isReal: Boolean = real

}
