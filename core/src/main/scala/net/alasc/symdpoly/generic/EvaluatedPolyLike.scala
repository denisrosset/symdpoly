package net.alasc.symdpoly
package generic

import shapeless.Witness
import spire.math.Rational

import cyclo.Cyclo

import net.alasc.symdpoly.evaluation.Evaluator

trait EvaluatedPolyLike[
  E <: Evaluator.Aux[M] with Singleton,
  M <: generic.MonoidDef with Singleton
] { lhs =>

  def toPoly: EvaluatedPoly[E, M]

  def <=!(rhs: EvaluatedPolyLike[E, M])(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] =
    ScalarConstraint(lhs.toPoly, ComparisonOp.LE, rhs.toPoly)

  def <=!(rhs: Int)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] = lhs <=! valueOf[E].constant(rhs)
  def <=!(rhs: Rational)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] = lhs <=! valueOf[E].constant(rhs)
  def <=!(rhs: Cyclo)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] = lhs <=! valueOf[E].constant(rhs)

  def >=!(rhs: EvaluatedPolyLike[E, M])(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] =
    ScalarConstraint(lhs.toPoly, ComparisonOp.GE, rhs.toPoly)

  def >=!(rhs: Int)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] = lhs >=! valueOf[E].constant(rhs)
  def >=!(rhs: Rational)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] = lhs >=! valueOf[E].constant(rhs)
  def >=!(rhs: Cyclo)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] = lhs >=! valueOf[E].constant(rhs)

  def =!(rhs: EvaluatedPolyLike[E, M])(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] =
    ScalarConstraint(lhs.toPoly, ComparisonOp.EQ, rhs.toPoly)

  def =!(rhs: Int)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] = lhs =! valueOf[E].constant(rhs)
  def =!(rhs: Rational)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] = lhs =! valueOf[E].constant(rhs)
  def =!(rhs: Cyclo)(implicit E: Witness.Aux[E], M: Witness.Aux[M]): ScalarConstraint[E, M] = lhs =! valueOf[E].constant(rhs)

  def maximize(implicit E: Witness.Aux[E], M: Witness.Aux[M]): Optimization[E, M] = new Optimization[E, M](Direction.Maximize, lhs.toPoly)

  def minimize(implicit E: Witness.Aux[E], M: Witness.Aux[M]): Optimization[E, M] = new Optimization[E, M](Direction.Minimize, lhs.toPoly)

}
