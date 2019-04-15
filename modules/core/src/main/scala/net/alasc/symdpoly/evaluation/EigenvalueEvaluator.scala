package net.alasc.symdpoly
package evaluation

import scala.collection.immutable.{HashSet}

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

/** Equivalence under the adjoint operation. */
final case class EigenvalueEvaluator[M <: generic.MonoDef with Singleton](real: Boolean, symmetryGroup: Grp[M#PermutationType])
                                                                         (implicit val witnessMono: Witness.Aux[M]) extends Evaluator {
  type Mono = M

  def apply(mono: Mono#MonoType): SingleMomentType = {
    val start: Set[Mono#MonoType] = if (real) HashSet(mono, mono.adjoint) else HashSet(mono)
    val candidates: Set[Mono#MonoType] = symmetrizeMonoSet(start, symmetryGroup)
    fromNormalForm(findCanonicalInSet(candidates))
  }

  protected def buildWithSymmetryGroup(newSymmetryGroup: Grp[M#PermutationType]): Evaluator.Aux[M] =
    new EigenvalueEvaluator[M](real, newSymmetryGroup)

  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = grp

  def isReal: Boolean = real

}
