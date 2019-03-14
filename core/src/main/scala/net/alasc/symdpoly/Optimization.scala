package net.alasc.symdpoly

import java.io.{BufferedWriter, FileWriter, StringWriter, Writer}

import net.alasc.symdpoly.ComparisonOp.{EQ, GE, LE}
import shapeless.Witness

import net.alasc.symdpoly.generic.{EvaluatedMono, EvaluatedPoly}
import net.alasc.symdpoly.util.{MemoMap, OrderedSet}
import scalin.immutable.{Mat, Vec, VecEngine}
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.vectorSpace._
import spire.syntax.involution._

import syntax.all._
import scalin.syntax.all._
import scalin.immutable.dense._
import scala.collection.immutable.{HashSet, SortedMap, SortedSet}

import spire.algebra.{Eq, Order, VectorSpace}
import spire.math.Complex
import spire.std.double._

import cyclo.{Cyclo, RealCyclo}

import net.alasc.symdpoly.SDP.Block
import net.alasc.symdpoly.algebra.NiceVectorSpace
import net.alasc.symdpoly.math.Phase

case class BlockElement(dualIndex: Int, r: Int, c: Int, realPart: Double, complexPart: Double)

class Relaxation[
  E <: generic.Evaluator.Aux[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](val direction: Direction,
  val objective: E#EvaluatedPolynomial,
  val momentMatrix: MomentMatrix[E, M],
  val scalarEq: Seq[E#EvaluatedPolynomial],
  val scalarIneq: Seq[E#EvaluatedPolynomial],
  val localizingMatrices: Seq[LocalizingMatrix[E, M]]) {

  def E: E = valueOf[E]

  case class DualTerm(dualIndex: Int, realPart: Double, imagPart: Double)

  lazy val (allMoments: OrderedSet[E#EvaluatedMonomial], adjointMoment: Array[Int], allSelfAdjoint: Boolean) = {
    val all: OrderedSet[E#EvaluatedMonomial] = OrderedSet.fromUnique(
      HashSet(E.one) union localizingMatrices.foldLeft(momentMatrix.allMoments) { case (s, lm) => s union lm.allMoments }
    )
    val adj = Array.tabulate(all.length) { i =>
      val m = all(i)
      val madj = E.evaluatedMonoInvolution.adjoint(m)
      val iadj = all.indexOf(madj)
      iadj
    }
    (all, adj, adj.zipWithIndex.forall { case (i, j) => i == j })
  }

  def expandSelfAdjoint(p: E#EvaluatedPolynomial): Vec[Double] = Vec.fromMutable(allMoments.length, 0.0) { vec =>
    cforRange(0 until p.nTerms) { pi =>
      val m = p.monomial(pi)
      val i = allMoments.indexOf(m)
      val iadj = adjointMoment(i)
      val beta = p.coeff(pi)
      if (i == iadj) {
        assert(beta.isReal)
        vec(i) := vec(i) + RealCyclo.real(beta).toAlgebraic.toDouble
      } else if (i < iadj) { // only consider half the self-adjoint terms
        // we have beta <m> + beta' <m'> =
        //   beta.real * m.real - beta.imag * m.imag + i * (beta.real * m.imag + beta.imag * m.real)
        //   beta.real * m.real - beta.imag * m.imag - i * (beta.real * m.imag + beta.imag * m.real)
        // = 2 * beta.real*m.real - 2 * beta.imag*m.imag
        val madj = allMoments(iadj)
        val betaAdj = p.coeff(madj)
        assert(betaAdj.adjoint === beta)
        val imagPart = RealCyclo.imag(beta).toAlgebraic.toDouble
        vec(i) := vec(i) + RealCyclo.real(beta * 2).toAlgebraic.toDouble
        vec(iadj) := vec(iadj) - RealCyclo.imag(beta * 2).toAlgebraic.toDouble
      }
    }
  }

  def inDualVariables(c: Cyclo, mono: E#EvaluatedMonomial): Seq[DualTerm] = if (mono.isZero) Seq.empty else {
    val canonical = mono.phaseCanonical
    val coeff = if (c.isOne) phaseValue(mono.phaseOffset) else cycloValue(c * mono.phaseOffset.toCyclo)
    val index = allMoments.indexOf(canonical)
    val indexAdjoint = adjointMoment(index)
    if (index == indexAdjoint) // monomial is self adjoint
      Seq(DualTerm(index, coeff.real, coeff.imag))
    else if (index < indexAdjoint) // monomial value is y(index) + i * y(indexAdjoint)
      Seq(
        DualTerm(index, coeff.real, coeff.imag),
        DualTerm(indexAdjoint, -coeff.imag, coeff.real) // (*) see below
      )
    else // monomial value is y(indexAdjoint) - i * y(index)
      Seq(
        DualTerm(indexAdjoint, coeff.real, coeff.imag),
        DualTerm(index, coeff.imag, -coeff.real) // opposite of the dual term in (*)
      )
  }

  def inDualVariables(poly: E#EvaluatedPolynomial): Seq[DualTerm] =
    (0 until poly.nTerms).flatMap(i => inDualVariables(poly.coeff(i), poly.monomial(i)))
      .groupBy(_.dualIndex)
      .values.map {
      case Seq(DualTerm(d, rp, ip), tl@_*) => DualTerm(d, tl.map(_.realPart).foldLeft(rp)(_+_), tl.map(_.imagPart).foldLeft(rp)(_+_))
    }.toSeq

  def expandMomentMatrix(mat: Mat[E#EvaluatedMonomial]): SDP.Block = {
    val nonZeroElements: Seq[BlockElement] = for {
      r <- 0 until mat.nRows
      c <- 0 until mat.nCols
      DualTerm(dualIndex, realPart, complexPart) <- inDualVariables(Cyclo.one, mat(r, c))
    } yield BlockElement(dualIndex, r, c, realPart, complexPart)
    SDP.Block(mat.nRows, nonZeroElements)
  }

  def expandLocalizingMatrix(mat: Mat[E#EvaluatedPolynomial]): SDP.Block = {
    val nonZeroElements: Seq[BlockElement] = for {
      r <- 0 until mat.nRows
      c <- 0 until mat.nCols
      poly = mat(r, c)
      i <- 0 until poly.nTerms
      mono = poly.monomial(i)
      coeff = poly.coeff(i)
      DualTerm(dualIndex, realPart, complexPart) <- inDualVariables(coeff, mono)
    } yield BlockElement(dualIndex, r, c, realPart, complexPart)
    SDP.Block(mat.nRows, nonZeroElements)
  }

  def expandRealPart(p: E#EvaluatedPolynomial): Vec[Double] = Vec.fromMutable(allMoments.length, 0.0) { vec =>
    cforRange(0 until p.nTerms) { pi =>
      val m = p.monomial(pi)
      val i = allMoments.indexOf(m)
      val iadj = adjointMoment(i)
      val beta = p.coeff(pi)
      if (i == iadj) {
        assert(beta.isReal)
        vec(i) := vec(i) + RealCyclo.real(beta).toAlgebraic.toDouble
      } else if (i < iadj) { // only consider half the self-adjoint terms
        // we have beta <m> + beta' <m'> =
        //   beta.real * m.real - beta.imag * m.imag + i * (beta.real * m.imag + beta.imag * m.real)
        //   beta.real * m.real - beta.imag * m.imag - i * (beta.real * m.imag + beta.imag * m.real)
        // = 2 * beta.real*m.real - 2 * beta.imag*m.imag
        val madj = allMoments(iadj)
        val betaAdj = p.coeff(madj)
        assert(betaAdj.adjoint === beta)
        val imagPart = RealCyclo.imag(beta).toAlgebraic.toDouble
        vec(i) := vec(i) + RealCyclo.real(beta * 2).toAlgebraic.toDouble
        vec(iadj) := vec(iadj) - RealCyclo.imag(beta * 2).toAlgebraic.toDouble
      }
    }
  }

  def expandLinear(p: E#EvaluatedPolynomial): Seq[Vec[Double]] = {
    val padj = E.evaluatedPolyInvolution.adjoint(p)
    val realPart = E.evaluatedPolyVectorSpace.divr(E.evaluatedPolyVectorSpace.plus(p, padj), Cyclo(2))
    val imagPart = E.evaluatedPolyVectorSpace.divr(E.evaluatedPolyVectorSpace.plus(p, padj), Cyclo(2))
    def optionExpand(sap: E#EvaluatedPolynomial): Seq[Vec[Double]] = if (sap.isZero) Seq.empty else Seq(expandRealPart(sap))
    optionExpand(realPart) ++ optionExpand(imagPart)
  }

  lazy val toSDP: SDP = {
    val m = allMoments.length
    // convention for the mapping of variables
    // for a moment of index i in allMoments, we distinguish two cases depending on iadj = adjointMoment(i)
    // - i == iadj: dualVariable(i) is the real value
    // - i < iadj: dualVariable(i) contains the real part, dualVariable(iadj) contains the imaginary part
    require(allMoments(0) == E.one)

    val obj = direction match {
      case Direction.Minimize => expandSelfAdjoint(E.evaluatedPolyVectorSpace.negate(objective))
      case Direction.Maximize => expandSelfAdjoint(objective)
    }

    val mainBlock = expandMomentMatrix(momentMatrix.mat)
    val localizingBlocks = localizingMatrices.map(lm => expandLocalizingMatrix(lm.mat))
    val blocks: Seq[SDP.Block] = mainBlock +: localizingBlocks
    val eqVecs = scalarEq.flatMap(expandLinear)
    val ineqVecs = scalarIneq.flatMap(expandLinear)
    val eqA = Mat.tabulate(eqVecs.size, m)( (r, c) => eqVecs(r)(c) )
    val ineqA = Mat.tabulate(eqVecs.size, m)( (r, c) => ineqVecs(r)(c) )
    SDP(direction, obj, blocks, eqA, ineqA)
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
    val generatingSet = gSet.toOrderedSet
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
    new Relaxation[E, M](direction, objective, momentMatrix, scalarEq, scalarIneq, localizingMatrices)
  }

}

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
