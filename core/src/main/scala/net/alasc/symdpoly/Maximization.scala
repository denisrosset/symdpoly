package net.alasc.symdpoly

import shapeless.Witness

import net.alasc.symdpoly.evaluation._

case class Maximization[
  E <: Evaluator[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](evaluatedPoly: EvaluatedPoly[E, M]) {

  def E: E = valueOf[E]
  def M: M = valueOf[M]

  def relaxation(generatingSet: GSet[M]): Relaxation[E, M] =
    Relaxation(this, generatingSet)

}
