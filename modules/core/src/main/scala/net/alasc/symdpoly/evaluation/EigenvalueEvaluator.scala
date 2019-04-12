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
final case class EigenvalueEvaluator[M <: generic.MonoidDef with Singleton](real: Boolean, symmetryGroup: Grp[M#PermutationType])
                                                                           (implicit val witnessMono: Witness.Aux[M]) extends Evaluator {
  type Mono = M

  lazy val symmetryGroupDecomposition: GrpDecomposition[Mono#PermutationType] = GrpDecomposition(symmetryGroup)

  protected def applyTransversal(elements: Set[Mono#MonoType],
                                 transversal: Vector[Mono#PermutationType]): Set[Mono#MonoType] =
    elements.flatMap(m => transversal.map(g => m <|+| g))

  def apply(mono: Mono#MonoType): SingleMomentType = {
    val start: Set[Mono#MonoType] = if (real) HashSet(mono, mono.adjoint) else HashSet(mono)
    val candidates: Set[Mono#MonoType] = symmetryGroupDecomposition.transversals.foldLeft(start) {
      case (set, transversal) => applyTransversal(set, transversal)
    }
    val canonical = candidates.map(_.phaseCanonical)
    if (canonical.size != candidates.size)
      new SingleMoment[this.type, Mono](M.monoMultiplicativeBinoid.zero)
    else
      new SingleMoment[this.type, Mono](candidates.qmin(M.monoOrder))
  }

  protected def buildWithSymmetryGroup(newSymmetryGroup: Grp[M#PermutationType]): Evaluator.Aux[M] =
    new EigenvalueEvaluator[M](real, newSymmetryGroup)

  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = grp

  def isReal: Boolean = real

}
