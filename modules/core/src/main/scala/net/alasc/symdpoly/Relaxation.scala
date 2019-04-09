package net.alasc.symdpoly

import java.io.{BufferedWriter, FileWriter, StringWriter, Writer}

import net.alasc.symdpoly.ComparisonOp.{EQ, GE, LE}
import shapeless.Witness

import net.alasc.symdpoly.generic.{SingleMoment, LinearMoment}
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

/** Moment-based relaxation of a noncommutative polynomial optimization problem. */
class Relaxation[
  E <: Evaluator.Aux[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](val direction: Direction,
  val objective: LinearMoment[E, M],
  val momentMatrix: MomentMatrix[E, M],
  val momentMatrixSymmetry: GrpMonomialRepresentation[M#PermutationType],
  val localizingMatrices: Seq[LocalizingMatrix[E, M]],
  val scalarEq: Seq[LinearMoment[E, M]],
  val scalarIneq: Seq[LinearMoment[E, M]]) {

  override def toString: String = s"Moment relaxation with ${allMoments.length-1} monomials, a moment matrix of size ${momentMatrix.size} x ${momentMatrix.size}, and ${localizingMatrices.size} localizing matrix/ces"

  def E: E = valueOf[E]

  lazy val (allMoments: OrderedSet[E#SingleMomentType], adjointMoment: Array[Int], allSelfAdjoint: Boolean) = {
    val all: OrderedSet[E#SingleMomentType] = OrderedSet.fromUnique(
      HashSet(E.one) union localizingMatrices.foldLeft(momentMatrix.allMoments) { case (s, lm) => s union lm.allMoments }
    )
    logVerbose(s"Found ${all.length} unique monomials in the relaxation")
    val adj = Array.tabulate(all.length) { i =>
      val m = all(i)
      val madj = E.evaluatedMonoInvolution.adjoint(m)
      val iadj = all.indexOf(madj)
      iadj
    }
    (all, adj, adj.zipWithIndex.forall { case (i, j) => i == j })
  }

  lazy val program: Program = {
    logNormal("Computing semidefinite program representation")
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

  /** Constructs a relaxation for the given optimization problem using the given generating set of monomials
    *
    * Currently, the construction of equality / inequality constraints / localizing matrices is very crude.
    */
  def apply[
    E <: Evaluator.Aux[M] with Singleton: Witness.Aux,
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](optimization: Optimization[E, M], gSet: GSet[M]): Relaxation[E, M] = {
    import optimization.{direction, objective}
    logNormal(s"Computing relaxation with the generating set ${gSet}")
    def E: E = valueOf[E]
    def M: M = valueOf[M]
    val generatingSet = gSet.toOrderedSet
    logNormal(s"Found ${generatingSet.length} generating monomials")
    val degree = generatingSet.iterator.map(_.degree).max
    logVerbose(s"Symmetry group of order ${E.symmetryGroup.order} provided")
    val compatible = symmetries.Orbit.compatibleSubgroup[M#MonoType, M#PermutationType](generatingSet.toIndexedSeq, E.symmetryGroup, M.monoPhased.phaseCanonical)
    if (compatible.order < E.symmetryGroup.order)
      logNormal(s"Warning: the generating set of monomials is not fully symmetric under the provided symmetry group, a subgroup of order ${compatible.order} is used instead")
    val symmetry = GrpMonomialRepresentation.fromActionOnOrderedSet(generatingSet, compatible)
    val momentMatrix = MomentMatrix[E, M](generatingSet, symmetry)
    def filterGeneratingSet(maxDegree: Int): OrderedSet[M#MonoType] =
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
    val scalarEqFromOp: Seq[E#LinearMomentType] = optimization.operatorConstraints.collect {
      case OperatorConstraint(lhs, EQ, rhs) => lhs - rhs
    }.flatMap { p =>
      val gs = filterGeneratingSet(degree - p.degree/2)
      for {
        r <- gs.iterator
        c <- gs.iterator
        res: M#PolyType = r.adjoint.toPoly * p * c.toPoly
      } yield E(res)
    }
    val scalarEq: Seq[E#LinearMomentType] = scalarEqFromOp ++ optimization.scalarConstraints.collect {
      case ScalarConstraint(lhs, EQ, rhs) => lhs - rhs
    }
    val scalarIneq: Seq[E#LinearMomentType] = optimization.scalarConstraints.collect {
      case ScalarConstraint(lhs, LE, rhs) => rhs - lhs
      case ScalarConstraint(lhs, GE, rhs) => lhs - rhs
    }
    new Relaxation[E, M](direction, objective, momentMatrix, symmetry, localizingMatrices, scalarEq, scalarIneq)
  }

}
