package net.alasc.symdpoly

import shapeless.Witness

import net.alasc.symdpoly.evaluation._

case class Maximization2[
  E <: Evaluator2[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](evaluatedPoly: EvaluatedPoly2[E, M]) {

  def E: E = valueOf[E]
  def M: M = valueOf[M]

  def relaxation(generatingSet: GSet[M]): Relaxation2[E, M] =
    Relaxation2(this, generatingSet)

}
