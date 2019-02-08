package net.alasc.symdpoly

import shapeless.Witness
import spire.algebra.{Action, Group, Involution}
import spire.syntax.cfor.cforRange

import cyclo.Cyclo
import scalin.immutable.{Mat, Vec}

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly
import net.alasc.symdpoly.algebra.{Instances, Morphism, MultiplicativeBinoid}
import net.alasc.symdpoly.evaluation._
import net.alasc.symdpoly.internal.{MomentSet, MomentSet2, MomentSetBuilder, MomentSetBuilder2}
import net.alasc.symdpoly.math.{GenPerm, PhasedInt, Phases}
import net.alasc.symdpoly.solvers.{MosekInstance, MosekInstance2, SDPAInstance, SDPAInstance2}
import net.alasc.util.Tuple2Int
import spire.syntax.multiplicativeMonoid._
import spire.syntax.involution._
import spire.std.unit._
import spire.syntax.action._

import net.alasc.symdpoly.algebra.Phased.syntax._
import scalin.immutable.dense._
import spire.std.int._

case class Maximization2[
  E <: Evaluator2[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](evaluatedPoly: EvaluatedPoly2[E, M]) {

  def E: E = valueOf[E]
  def M: M = valueOf[M]

  def relaxation(generatingSet: GSet[M]): Relaxation2[E, M] =
    Relaxation2(this, generatingSet)

}

case class Relaxation2[
  E <: Evaluator2[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux,
](problem: Maximization2[E, M], generatingSet: GSet[M]) {

  val objective: EvaluatedPoly2[E, M] =
    valueOf[E].apply(problem.evaluatedPoly.normalForm)

  val gramMatrix: GramMatrix2[E, M] = GramMatrix2(valueOf[E], generatingSet)

  val objectiveVector: Vec[Cyclo] = objective.vecOverOrderedSet(gramMatrix.momentSet.elements)

  def isObjectiveReal: Boolean = objectiveVector.toIndexedSeq.forall(c => c.isReal)

  def mosekInstance: MosekInstance2 = new MosekInstance2(this)
  def sdpaInstance: SDPAInstance2 = new SDPAInstance2(this)

}

class GramMatrix2[
  E <: Evaluator2[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux
](val generatingMoments: OrderedSet[M#Monomial],
  val momentSet: MomentSet2[E, M],
  private[this] val momentArray: Array[Int],
  private[this] val phaseArray: Array[Int] // phase encoding
 ) {
  def E: E = valueOf[E]
  def M: M = valueOf[M]

  def isReal: Boolean = {
    cforRange(0 until phaseArray.length) { i =>
      if (phaseArray(i) != Phase.one.encoding && phaseArray(i) != Phase.minusOne.encoding)
        return false
    }
    momentSet.allSelfAdjoint
  }

  val matrixSize = generatingMoments.length

  private[this] def inMat(r: Int, c: Int): Int = r + c * matrixSize

  scalin.immutable.dense.matEngine[Int]

  def momentIndexMatrix: Mat[Int] = Mat.tabulate(matrixSize, matrixSize) { (r, c) => momentIndex(r, c)}
  def phaseMatrix: Mat[Phase] = Mat.tabulate(matrixSize, matrixSize) { (r, c) => phase(r, c) }

  def momentIndex(r: Int, c: Int): Int = momentArray(inMat(r, c))

  def absMoment(r: Int, c: Int): EvaluatedMono2[E, M] = momentIndex(r, c) match {
    case -1 => E.evaluatedMonoZero
    case i => momentSet(i)
  }

  def moment(r: Int, c: Int): EvaluatedMono2[E, M] = absMoment(r, c) <* phase(r, c)
  def phase(r: Int, c: Int): Phase = Phase.fromEncoding(phaseArray(inMat(r, c)))
  def nUniqueMonomials: Int = momentSet.nElements

  def momentMatrix: Mat[EvaluatedMono2[E, M]] = Mat.tabulate(matrixSize, matrixSize) { (r, c) => moment(r, c) }

}

object GramMatrix2 {

  def apply[
    E <: Evaluator2[M] with Singleton,
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](E: E, gSet: GSet[M]): GramMatrix2[E, M] = {
    implicit def witnessE: Witness.Aux[E] = (E: E).witness
    def M: M = valueOf[M]
    val generatingMoments = OrderedSet.fromOrdered(gSet.monomials.toVector)
    val n = generatingMoments.length
    def inMat(r: Int, c: Int): Int = r + c * n
    val phaseMatrix = Array.fill[Int](n * n)(Phase.one.encoding)
    val unsortedMomentMatrix = Array.fill[Int](n * n)(Int.MinValue)
    val sb = MomentSetBuilder2.make[E, M]
    cforRange(0 until n) { r =>
      cforRange(r until n) { c =>
        if (unsortedMomentMatrix(inMat(r, c)) == Int.MinValue) {
          implicit def involution: Involution[M#Monomial] = M.monoInvolution
          implicit def monoMultiplicativeBinoid: MultiplicativeBinoid[M#Monomial] = M.monoMultiplicativeBinoid
          val phased: EvaluatedMono2[E, M] = (E: E)(generatingMoments(r).adjoint * generatingMoments(c))
          val phase = phased.phaseOffset
          val canonical = phased.phaseCanonical
          // TODO: check phase support when complex is supported
          if (phased.isZero) {
            unsortedMomentMatrix(inMat(r, c)) = -1
            unsortedMomentMatrix(inMat(c, r)) = -1
          } else if (r == c || E.isSelfAdjoint) {
            val index = sb.getElement(canonical)
            unsortedMomentMatrix(inMat(r, c)) = index
            unsortedMomentMatrix(inMat(c, r)) = index
            phaseMatrix(inMat(r, c)) = phase.encoding
            phaseMatrix(inMat(c, r)) = phase.encoding
          } else {
            val phasedAdj = (E: E)(generatingMoments(c).adjoint * generatingMoments(r))
            val phaseAdj = phasedAdj.phaseOffset
            val canonicalAdj = phasedAdj.phaseCanonical
            val tuple = sb.getElement(canonical, canonicalAdj)
            unsortedMomentMatrix(inMat(r, c)) = tuple._1
            unsortedMomentMatrix(inMat(c, r)) = tuple._2
            phaseMatrix(inMat(r, c)) = phase.encoding
            phaseMatrix(inMat(c, r)) = phaseAdj.encoding
          }
        }
      }
    }
    val (sortedMoments, unsortedToSorted) = sb.result()
    val sortedMomentMatrix = unsortedMomentMatrix.map {
      case -1 => -1
      case i => unsortedToSorted.image(i)
    }
    new GramMatrix2[E, M](generatingMoments, sortedMoments, sortedMomentMatrix, phaseMatrix)
  }
}
