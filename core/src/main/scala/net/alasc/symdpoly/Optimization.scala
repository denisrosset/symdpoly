package net.alasc.symdpoly

import net.alasc.symdpoly.ComparisonOp.{EQ, GE, LE}
import shapeless.Witness
import net.alasc.symdpoly.generic.{EvaluatedMono, EvaluatedPoly}
import net.alasc.symdpoly.util.OrderedSet
import scalin.immutable.{Mat, Vec}
import spire.syntax.cfor._
import spire.syntax.vectorSpace._
import spire.syntax.involution._
import scalin.immutable.dense._

import scala.collection.immutable.SortedSet

/** Direction along which to optimize the objective: minimization or maximization. */
sealed trait Direction

object Direction {
  case object Minimize extends Direction
  case object Maximize extends Direction
}

case class DualSDP(obj: Vec[Double], blocks: Seq[DualSDP.Block]) {
  def m: Int = obj.length
}

object DualSDP {

  case class Block(basis: Seq[Mat[Double]])

}

class Relaxation[
  E <: generic.Evaluator.Aux[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](val direction: Direction,
  val objective: EvaluatedPoly[E, M],
  val momentMatrix: MomentMatrix[E, M],
  val scalarEq: Seq[EvaluatedPoly[E, M]],
  val scalarIneq: Seq[EvaluatedPoly[E, M]],
  val localizingMatrices: Seq[LocalizingMatrix[E, M]]) {

  def E: E = valueOf[E]

  lazy val (allMoments: OrderedSet[EvaluatedMono[E, M]], adjointMoment: Array[Int]) = {
    val all: OrderedSet[EvaluatedMono[E, M]] =
      OrderedSet(E.one) union localizingMatrices.foldLeft(momentMatrix.allMoments) { case (s, lm) => s union lm.allMoments }
    val adj = Array.tabulate(all.length) { i =>
      val m = all(i)
      val madj = E.evaluatedMonoInvolution.adjoint(m)
      val iadj = all.indexOf(madj)
      iadj
    }
    (all, adj)
  }

  def toSDP: DualSDP = {

    def vec(p: E#EvaluatedPolynomial): Vec[Double] = {
      Vec.fromMutable(allMoments.length)
    }
  }

}

object Relaxation {

  def apply[
    E <: generic.Evaluator.Aux[M] with Singleton: Witness.Aux,
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](optimization: Optimization[E, M], gSet: GSet[M], optimize: Boolean): Relaxation[E, M] = {
    import optimization.{objective, direction}
    def E: E = valueOf[E]
    import generic.MonoidDef.polyAssociativeAlgebra
    import generic.Evaluator.evaluatedPolyVectorSpace
    val generatingSet = gSet.monomials
    val degree = generatingSet.iterator.map(_.degree).max
    val momentMatrix = MomentMatrix[E, M](generatingSet, optimize)
    def filterGeneratingSet(maxDegree: Int): OrderedSet[M#Monomial] =
      OrderedSet.fromSortedSet(
        generatingSet.toSortedSet.filter(_.degree <= maxDegree)
      )
    val localizingMatrices = optimization.operatorConstraints.collect {
      case OperatorConstraint(lhs, LE, rhs) =>
        val p = rhs - lhs
        LocalizingMatrix(p, filterGeneratingSet(degree - p.degree/2))
      case OperatorConstraint(lhs, GE, rhs) =>
        val p = lhs - rhs
        LocalizingMatrix(p, filterGeneratingSet(degree - p.degree/2))
    }
    val scalarEqFromOp: Seq[E#EvaluatedPolynomial] = optimization.operatorConstraints.collect {
      case OperatorConstraint(lhs, EQ, rhs) => lhs - rhs
    }.flatMap { p =>
        val gs = filterGeneratingSet(degree - p.degree/2)
        for {
          r <- gs.iterator
          c <- gs.iterator
          res: M#Polynomial = r.adjoint.toPoly * p * c.toPoly
        } yield E(res)
    }
    val scalarEq: Seq[E#EvaluatedPolynomial] = scalarEqFromOp ++ optimization.scalarConstraints.collect {
      case ScalarConstraint(lhs, EQ, rhs) => lhs - rhs
    }
    val scalarIneq: Seq[E#EvaluatedPolynomial] = optimization.scalarConstraints.collect {
      case ScalarConstraint(lhs, LE, rhs) => rhs - lhs
      case ScalarConstraint(lhs, GE, rhs) => lhs - rhs
    }
    new Relaxation(direction, objective, momentMatrix, scalarEq, scalarIneq, localizingMatrices)
  }

}

/** Polynomial optimization problem. */
case class Optimization[
  E <: generic.Evaluator.Aux[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](direction: Direction, objective: EvaluatedPoly[E, M], operatorConstraints: Seq[OperatorConstraint[M]] = Seq.empty, scalarConstraints: Seq[ScalarConstraint[E, M]] = Seq.empty) {

  def E: E = valueOf[E]
  def M: M = valueOf[M]

  /** Constructs a moment-based/SOS relaxation. */
  def relaxation(generatingSet: GSet[M], optimize: Boolean = true): Relaxation[E, M] = Relaxation(this, generatingSet, optimize)

  /** Constructs a moment-based/SOS relaxation. */
  def oldRelaxation(generatingSet: GSet[M]): OldRelaxation[E, M] = OldRelaxation(this, generatingSet)

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
