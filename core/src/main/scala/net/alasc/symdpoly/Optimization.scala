package net.alasc.symdpoly

import shapeless.Witness

import net.alasc.symdpoly.generic.EvaluatedPoly

/** Direction along which to optimize the objective: minimization or maximization. */
sealed trait Direction

object Direction {
  case object Minimize extends Direction
  case object Maximize extends Direction
}

/** Polynomial optimization problem. */
case class Optimization[
  E <: generic.Evaluator[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](direction: Direction, evaluatedPoly: EvaluatedPoly[E, M], constraints: Seq[Constraint[E, M]] = Seq.empty) {

  def E: E = valueOf[E]
  def M: M = valueOf[M]

  /** Constructs a moment-based/SOS relaxation. */
  def relaxation(generatingSet: GSet[M]): Relaxation[E, M] =
    Relaxation(this, generatingSet)

  def subjectTo(additionalConstraints: Constraint[E, M]*): Optimization[E, M] =
    Optimization(direction, evaluatedPoly, constraints ++ additionalConstraints)

}
