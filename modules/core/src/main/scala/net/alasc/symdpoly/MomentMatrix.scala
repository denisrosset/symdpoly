package net.alasc.symdpoly

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

import shapeless.Witness
import spire.algebra.Group

import scalin.immutable.Mat

import syntax.phased._
import spire.syntax.involution._
import spire.syntax.cfor._

import net.alasc.perms.Perm
import spire.syntax.ring._
import spire.syntax.action._

import net.alasc.symdpoly.util.OrderedSet
import scalin.immutable.dense._
import scalin.syntax.all._

import net.alasc.perms.default._
import cern.colt.matrix.io.MatrixInfo.MatrixSymmetry
import net.alasc.finite.Grp
import net.alasc.symdpoly.algebra.Morphism
import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.symdpoly.math.{GenPerm, GrpMonomialRepresentation, Phase}
import net.alasc.symdpoly.sdp.{BasisTerm, Block, BlockElement}
import net.alasc.symdpoly.symmetries.Configuration

/** Moment matrix
  *
  * @param generatingMoments Generating moments of this moment matrix
  * @param mat Matrix of evaluated moments, such that moments(r, c) = E(generatingMoments(r).adjoint * generatingMoments(c))
  * @tparam E Evaluator
  * @tparam M Monomial monoid
  */
class MomentMatrix[
  E <: Evaluator.Aux[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](val generatingMoments: OrderedSet[M#MonoType], val mat: Mat[E#SingleMomentType]) {
  def E: E = valueOf[E]
  def M: M = valueOf[M]

  /** The matrix of moments has shape size x size */
  def size: Int =  generatingMoments.length

  def allMoments: HashSet[E#SingleMomentType] = MomentMatrix.matIterator(mat).map(_.phaseCanonical).filterNot(_.isZero).to[HashSet]

  def expandIn(relaxation: Relaxation[E, M]): (Block, Boolean) = {
    val nonZeroElements: Seq[BlockElement] = for {
      r <- 0 until mat.nRows
      c <- 0 until mat.nCols
      BasisTerm(dualIndex, realPart, complexPart) <- mat(r, c).expandIn(relaxation)
    } yield BlockElement(dualIndex, r, c, realPart, complexPart)
    val hasComplexEncoding = nonZeroElements.exists(_.complexPart != 0)
    (Block(mat.nRows, relaxation.allMoments.length, nonZeroElements), hasComplexEncoding)
  }

}

object MomentMatrix {

  def matIterator[A](mat: Mat[A]): Iterator[A] = for {
    r <- Iterator.range(0, mat.nRows)
    c <- Iterator.range(0, mat.nCols)
  } yield mat(r, c)

  def apply[
    E <: Evaluator.Aux[M] with Singleton : Witness.Aux,
    M <: generic.MonoidDef with Singleton : Witness.Aux
  ](generatingMoments: OrderedSet[M#MonoType]): MomentMatrix[E, M] =
    apply[E, M](generatingMoments, GrpMonomialRepresentation.trivial[M#PermutationType](generatingMoments.length))

  /** Constructs a moment matrix according to the given generating moments and the given evaluator.
    * The symmetry of the moment matrix can be provided when the evaluator has equivalence under symmetry.
    */
  def apply[
    E <: Evaluator.Aux[M] with Singleton : Witness.Aux,
    M <: generic.MonoidDef with Singleton : Witness.Aux
  ](generatingMoments: OrderedSet[M#MonoType], symmetry: GrpMonomialRepresentation[M#PermutationType]): MomentMatrix[E, M] = {
    def E: E = valueOf[E]
    def M: M = valueOf[M]
    val size = generatingMoments.length
    val symmetry = GrpMonomialRepresentation.fromActionOnOrderedSet(generatingMoments, E.symmetryGroup: Grp[M#PermutationType])
    val moments: Mat[E#SingleMomentType] =
      if (symmetry.grp.isTrivial)
        Mat.tabulate(size, size) { (r, c) => E(generatingMoments(r).adjoint * generatingMoments(c)) }
      else
        Mat.fromMutable[E#SingleMomentType](size, size, E.zero) { mat =>
          val conf = Configuration.fromGrpMonomialRepresentation(symmetry)
          cforRange(0 until conf.nOrbits) { o =>
            val ptr: symmetries.Ptr[conf.type] = conf.orbitStart(o)
            val r = ptr.row
            val c = ptr.col

            val v = E(generatingMoments(ptr.row).adjoint * generatingMoments(ptr.col))
            // cache monomials after phase action
            val map = scala.collection.mutable.LongMap(Phase.one.encoding.toLong -> v)

            mat(r, c) := v

            @tailrec def rec(p: symmetries.Ptr[conf.type]): Unit =
              if (!p.isEmpty) {
                mat(p.row, p.col) := map.getOrElseUpdate(p.phase.encoding.toLong, v <* p.phase)
                rec(p.next)
              }

            rec(ptr.next)
          }
        }
    new MomentMatrix[E, M](generatingMoments, moments)
  }

}
