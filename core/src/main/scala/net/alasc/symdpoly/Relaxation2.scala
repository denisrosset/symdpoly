package net.alasc.symdpoly

import shapeless.Witness

import cyclo.Cyclo
import scalin.immutable.Vec

import net.alasc.symdpoly.evaluation.{EvaluatedPoly2, Evaluator2}
import net.alasc.symdpoly.solvers.{MosekInstance2, SDPAInstance2}
import scalin.immutable.dense._

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
