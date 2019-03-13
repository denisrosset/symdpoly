package net.alasc.symdpoly

import net.alasc.symdpoly.ComparisonOp.{EQ, GE, LE}
import shapeless.Witness

import net.alasc.symdpoly.generic.{EvaluatedMono, EvaluatedPoly}
import net.alasc.symdpoly.util.OrderedSet
import scalin.immutable.{Mat, Vec, VecEngine}
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.vectorSpace._
import spire.syntax.involution._

import syntax.all._
import scalin.syntax.all._
import scalin.immutable.dense._
import scala.collection.immutable.{SortedMap, SortedSet}

import spire.algebra.{Eq, Order, VectorSpace}
import spire.math.Complex
import spire.std.double._
import cyclo.{Cyclo, RealCyclo}

import net.alasc.symdpoly.SDP.Block
import net.alasc.symdpoly.algebra.NiceVectorSpace
import net.alasc.symdpoly.math.Phase


/** Direction along which to optimize the objective: minimization or maximization. */
sealed trait Direction

object Direction {
  case object Minimize extends Direction
  case object Maximize extends Direction
}

/** Description of an semidefinite program extended dual.
  *
  * The conic linear program is given by:
  *
  *   maximize   sum_i objToMaximize(i) * y(i)
  *   over real y(0), ..., y(m-1)
  *
  *   subject to
  *
  *   y(0) == 1
  *   for all j: sum_i y(i) blocks(j).basis(i) >= 0
  *   eqA * y == 0
  *   ineqA * y >= 0 (component-wise)
  */
case class SDP(objToMaximize: Vec[Double], blocks: Seq[SDP.Block], eqA: Mat[Double], ineqA: Mat[Double]) {
  def m: Int = objToMaximize.length
}

object SDP {

  /*
  def expandOverFiniteBasis[V, B, F](basis: OrderedSet[B], v: V)(implicit V: NiceVectorSpace[V, B, F], F: VecEngine[F]): Vec[F] =
    F.fromMutable(basis.length, V.scalar.zero) { vec =>
      import V.scalar

    }*/

  case class Block(basisIndex: Array[Int], rowIndex: Array[Int], colIndex: Array[Int], coeffs: Array[Double])

}

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

  lazy val (allMoments: OrderedSet[E#EvaluatedMonomial], adjointMoment: Array[Int], allSelfAdjoint: Boolean) = {
    val all: OrderedSet[E#EvaluatedMonomial] =
      OrderedSet(E.one) union localizingMatrices.foldLeft(momentMatrix.allMoments) { case (s, lm) => s union lm.allMoments }
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

  private[this] val cycloValues = scala.collection.mutable.HashMap[Cyclo, Complex[Double]](
    Cyclo.zero -> Complex.zero[Double],
    Cyclo.one -> Complex.one[Double],
    Cyclo.minusOne -> Complex.fromInt[Double](-1),
    Cyclo.i -> Complex.i[Double],
    (-Cyclo.i) -> -Complex.i[Double]
  )
  private[this] val phaseValues = scala.collection.mutable.HashMap[Phase, Complex[Double]]

  def cycloValue(c: Cyclo): Complex[Double] = cycloValues.getOrElseUpdate(c, c => Complex(RealCyclo.real(c).toAlgebraic.toDouble, RealCyclo.imag(c).toAlgebraic.toDouble))
  def phaseValue(p: Phase): Complex[Double] = phaseValues.getOrElseUpdate(p, p => cycloValue(p.toCyclo))

  case class BlockElement(dualIndex: Int, r: Int, c: Int, realPart: Double, complexPart: Double)

  def realBlock(elements: Seq[BlockElement]): SDP.Block =
    Block(elements.map(_.dualIndex).toArray, elements.map(_.r).toArray, elements.map(_.c).toArray, elements.map(_.realPart).toArray)

  def complexBlock(elements: Seq[BlockElement]): SDP.Block = {
    def realPart(i: Int, r: Int, c: Int, a: Double) = Seq(
      (i, r*2, c*2, a),
      (i, r*2+1, c*2+1, a)
    )
    def imagPart(i: Int, r: Int, c: Int, b: Double) = Seq(
      (i, r*2, c*2+1, -b),
      (i, r*2+1, c*2, b)
    )
    val complexEncoding = elements.flatMap {
      case BlockElement(i, r, c, 0.0, b) => imagPart(i, r, c, b)
      case BlockElement(i, r, c, a, 0.0) => realPart(i, r, c, a)
      case BlockElement(i, r, c, a, b) => realPart(i, r, c, a) ++ imagPart(i, r, c, b)
    }
    realBlock(complexEncoding)
  }

  /** Constructs a SDP block from a series of indices.
    *
    * Assumes that no two elements have the same (dualIndex, r, c) value.
    */
  def block(elements: Seq[BlockElement]): SDP.Block =
    if (elements.forall(_.complexPart == 0)) realBlock(elements) else complexBlock(elements)

  def expandLocalizingMatrix(mat: Mat[E#EvaluatedPolynomial]): SDP.Block = {

  }

  def expandMomentMatrix(mat: Mat[E#EvaluatedMonomial]): SDP.Block = {
    /*val phases: Mat[Phase] = mat.map( e => e.phaseOffset )
    val canonical = mat.map( e => e.phaseCanonical )
    if (allSelfAdjoint && MomentMatrix.matIterator(phases).forall(_.isReal)) {*/

    val nonZeroElements: Seq[(Int, Int, Int, Phase)] = for {
        r <- 0 until mat.nRows
        c <- 0 until mat.nCols
        canonical = mat(r, c).phaseCanonical if !canonical.isZero
        phase = mat(r, c).phaseOffset
        momentIndex = allElements.indexOf(canonical)
      } yield (momentIndex, r, c, phase)
    if (allSelfAdjoint && nonZeroElements.forall(_._4.isReal)) { // all moments are real, phases are all real
      val basisIndex = nonZeroElements.map(_._1).toArray
      val rowIndex = nonZeroElements.map(_._2).toArray
      val colIndex = nonZeroElements.map(_._3).toArray
      val coeffs = nonZeroElements.map(_._4.toDouble).toArray
      Block(basisIndex, rowIndex, colIndex, coeffs)
    } else {
      // complex encoding with
      // [real -imag; imag real]
      val complexEncoding = nonZeroElements.flatMap {
        case (b, r, c, phase) if adjointMoment(b) == b && phase.isReal =>
          Seq(
            (b, r*2, c*2, phase.toDouble),
            (b, r*2+1, c*2+1, phase.toDouble)
          )
        case (b, r, c, phase) if b < adjointMoment(b) =>
          val badj = adjointMoment(b)
          // the value of the monomial b is encoded as y(b) + i * y(badj)
          // we consider the product
          // y(b) * phaseR - y(badj) * phaseI + i * (y(b) * phaseI + y(badj) * phaseR)
          val phaseR = RealCyclo.real(phase.toCyclo).toAlgebraic.toDouble
          val phaseI = RealCyclo.imag(phase.toCyclo).toAlgebraic.toDouble
          Seq(
            (b, r*2, c*2, phaseR),     // y(b) * phaseR
            (b, r*2+1, c*2+1, phaseR),
            (badj, r*2, c*2, -phaseI), // -y(badj) * phaseI
            (badj, r*2+1, c*2+1, -phaseI),
            (b, r*2, c*2+1, -phaseI), // i*y(b)*phaseI, but with sign change due to algebra encoding
            (b, r*2+1, c*2, phaseI), // no sign change
            (badj, r*2, c*2+1, -phaseR), // i*y(badj)*phaseR, but with sign change
            (badj, r*2+1, c*2, phaseR) // no sign change
          )
      }
      val basisIndex = complexEncoding.map(_._1).toArray
      val rowIndex = complexEncoding.map(_._2).toArray
      val colIndex = complexEncoding.map(_._3).toArray
      val coeffs = complexEncoding.map(_._4).toArray
      Block(basisIndex, rowIndex, colIndex, coeffs)
    }
  }

  def toSDP: SDP = {
    lazy val m = allMoments.length
    // convention for the mapping of variables
    // for a moment of index i in allMoments, we distinguish two cases depending on iadj = adjointMoment(i)
    // - i == iadj: dualVariable(i) is the real value
    // - i < iadj: dualVariable(i) contains the real part, dualVariable(iadj) contains the imaginary part
    require(allMoments(0) == E.one)

    val obj = direction match {
      case Direction.Minimize => expandSelfAdjoint(-objective)
      case Direction.Maximize => expandSelfAdjoint(objective)
    }

    val mainBlock = expandMomentMatrix(momentMatrix.mat)

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
