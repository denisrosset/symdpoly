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

import cats.arrow.Compose
import spire.algebra.{Eq, Group, Order, VectorSpace}
import spire.math.Complex
import spire.std.double._

import syntax.phased._
import cyclo.{Cyclo, RealCyclo}

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly.algebra.Morphism.GroupMorphism
import net.alasc.symdpoly.algebra.{Morphism, NiceVectorSpace, Phased}
import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.symdpoly.math.{GenPerm, GrpMonomialRepresentation, Phase}
import sdp._
import net.alasc.perms.default._

class Relaxation[
  E <: Evaluator.Aux[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](val direction: Direction,
  val objective: EvaluatedPoly[E, M],
  val momentMatrix: MomentMatrix[E, M],
  val momentMatrixSymmetry: GrpMonomialRepresentation[M#Permutation],
  val localizingMatrices: Seq[LocalizingMatrix[E, M]],
  val scalarEq: Seq[EvaluatedPoly[E, M]],
  val scalarIneq: Seq[EvaluatedPoly[E, M]]) {

  override def toString: String = s"Moment relaxation with ${allMoments.length} monomials, a moment matrix of size ${momentMatrix.size} x ${momentMatrix.size}, and ${localizingMatrices.size} localizing matrix/ces"

  def E: E = valueOf[E]

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

  lazy val program: Program = {
    val m = allMoments.length
    // convention for the mapping of variables
    // for a moment of index i in allMoments, we distinguish two cases depending on iadj = adjointMoment(i)
    // - i == iadj: dualVariable(i) is the real value
    // - i < iadj: dualVariable(i) contains the real part, dualVariable(iadj) contains the imaginary part
    require(allMoments(0) == E.one)

    val obj = objective.vectorSelfAdjointIn(this)
    val (momentBlock, momentBlockIsComplex) = momentMatrix.expandIn(this)
    val localizingBlocks = localizingMatrices.map(_.expandIn(this)._1)
    val blocks: Seq[Block] = momentBlock +: localizingBlocks
    def repMat(g: GenPerm): RepMat = {
      val momentMat: GenPermMat = if (momentBlockIsComplex) GenPermMat.ComplexMat(momentBlock.size, g) else GenPermMat.RealMat(momentBlock.size, g)
      val localizingMats: Seq[GenPermMat] = localizingBlocks.map(b => GenPermMat.RealMat(b.size, GenPerm.id))
      RepMat(momentMat +: localizingMats)
    }
    val toRepMat = Morphism[GenPerm, RepMat, Group](repMat)(implicitly, RepMat.groupInstance(repMat(GenPerm.id)))
    val permSymmetry = momentMatrixSymmetry.onPermutationGroup
    val permGrp = permSymmetry.grp
    val permRepresentation = Compose[GroupMorphism].andThen(permSymmetry.representation, toRepMat)

    val eqVecs = scalarEq.flatMap(_.vectorRealImagPartsIn(this))
    val ineqVecs = scalarIneq.map(_.vectorSelfAdjointIn(this))
    val eqA = Mat.tabulate(eqVecs.size, m)((r, c) => eqVecs(r)(c))
    val ineqA = Mat.tabulate(ineqVecs.size, m)((r, c) => ineqVecs(r)(c))

    Program(direction, obj, SDPConstraint(m, blocks, permGrp, permRepresentation), eqA, ineqA)
  }

}

object Relaxation {

  def apply[
    E <: Evaluator.Aux[M] with Singleton: Witness.Aux,
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](optimization: Optimization[E, M], gSet: GSet[M], optimize: Boolean): Relaxation[E, M] = {
    import optimization.{direction, objective}
    def E: E = valueOf[E]
    def M: M = valueOf[M]
    val generatingSet = gSet.toOrderedSet
    val degree = generatingSet.iterator.map(_.degree).max
    val compatible = symmetries.Orbit.compatibleSubgroup[M#Monomial, M#Permutation](generatingSet.toIndexedSeq, E.symmetryGroup, M.monoPhased.phaseCanonical)
    val symmetry = GrpMonomialRepresentation.fromActionOnOrderedSet(generatingSet, compatible)
    val momentMatrix = MomentMatrix[E, M](generatingSet, symmetry)
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
    new Relaxation[E, M](direction, objective, momentMatrix, symmetry, localizingMatrices, scalarEq, scalarIneq)
  }

}