package net.alasc.symdpoly

import shapeless.Witness

import net.alasc.symdpoly.generic.{EvaluatedPoly, Evaluator}

sealed trait Constraint[+E <: Evaluator.Aux[M] with Singleton, M <: generic.MonoidDef with Singleton]

final case class OperatorConstraint[
  M <: generic.MonoidDef with Singleton:Witness.Aux
](lhs: M#Polynomial, op: ComparisonOp, rhs: M#Polynomial) extends Constraint[Nothing, M]

final case class ScalarConstraint[
  E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux
](lhs: EvaluatedPoly[E, M], op: ComparisonOp, rhs: EvaluatedPoly[E, M]) extends Constraint[E, M]
