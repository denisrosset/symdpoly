package net.alasc.symdpoly

import shapeless.Witness

/** Polynomial optimization problem. */
case class Optimization[
  E <: generic.Evaluator.Aux[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](direction: Direction, objective: E#EvaluatedPolynomial, operatorConstraints: Seq[OperatorConstraint[M]] = Seq.empty, scalarConstraints: Seq[ScalarConstraint[E, M]] = Seq.empty) {

  def E: E = valueOf[E]
  def M: M = valueOf[M]

  /** Constructs a moment-based/SOS relaxation. */
  def relaxation(generatingSet: GSet[M], optimize: Boolean = true): Relaxation[E, M] = Relaxation(this, generatingSet, optimize)

  def subjectTo(newConstraints: Constraint[E, M]*): Optimization[E, M] = {
    val newOperatorConstraints = newConstraints.collect {
      case c: OperatorConstraint[M] => c
    }
    val newScalarConstraints = newConstraints.collect {
      case c: ScalarConstraint[E, M] => c
    }
    Optimization(direction, objective, operatorConstraints ++ newOperatorConstraints, scalarConstraints ++ newScalarConstraints)
  }

}
