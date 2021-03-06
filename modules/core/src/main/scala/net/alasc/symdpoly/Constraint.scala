package net.alasc.symdpoly

import shapeless.Witness
import spire.syntax.involution._
import spire.syntax.eq._
import generic.LinearMoment

sealed trait Constraint[+E <: evaluation.Evaluator.Aux[M] with Singleton, M <: generic.MonoDef with Singleton]

final case class OperatorConstraint[
  M <: generic.MonoDef with Singleton:Witness.Aux
](lhs: M#PolyType, op: ComparisonOp, rhs: M#PolyType) extends Constraint[Nothing, M] {
  def M: M = valueOf[M]
  require(op == ComparisonOp.EQ || {
    val diff = M.polyAssociativeAlgebra.minus(lhs, rhs)
    M.polyEq.eqv(M.polyInvolution.adjoint(diff), diff)
  }, "Operator inequality constraints can only be constructed using self-adjoint polynomials")

}

final case class ScalarConstraint[
  E <: evaluation.Evaluator.Aux[M] with Singleton:Witness.Aux,
  M <: generic.MonoDef with Singleton:Witness.Aux
](lhs: LinearMoment[E, M], op: ComparisonOp, rhs: LinearMoment[E, M]) extends Constraint[E, M] {
  def E: E = valueOf[E]
  require(op == ComparisonOp.EQ || {
    val diff = E.evaluatedPolyVectorSpace.minus(lhs, rhs)
    E.evaluatedPolyEq.eqv(E.evaluatedPolyInvolution.adjoint(diff), diff)
  }, "Scalar inequality constraints can only be constructed using self-adjoint polynomials")
}
