package net.alasc.symdpoly

import scala.annotation.tailrec

import shapeless.Witness

import scalin.immutable.Mat
import syntax.phased._
import spire.syntax.involution._
import spire.syntax.cfor._
import net.alasc.perms.Perm
import spire.syntax.ring._
import spire.syntax.action._
import net.alasc.symdpoly.symmetries.MatrixSymmetries
import net.alasc.symdpoly.util.OrderedSet
import scalin.immutable.dense._
import scalin.syntax.all._

/** Moment matrix
  *
  * @param generatingMoments Generating moments of this moment matrix
  * @param symmetries Symmetries of the moment matrix, as described by generalized permutations
  * @param mat Matrix of evaluated moments, such that moments(r, c) = E(generatingMoments(r).adjoint * generatingMoments(c))
  * @tparam E Evaluator
  * @tparam M Monomial monoid
  */
class MomentMatrix[
  E <: generic.Evaluator.Aux[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](val generatingMoments: OrderedSet[M#Monomial], val mat: Mat[E#EvaluatedMonomial], val symmetries: MatrixSymmetries[Perm]) {
  def E: E = valueOf[E]
  def M: M = valueOf[M]

  /** The matrix of moments has shape size x size */
  def size: Int =  generatingMoments.length

  def allMoments: OrderedSet[E#EvaluatedMonomial] = OrderedSet.fromIterator(MomentMatrix.matIterator(mat).map(_.phaseCanonical).filterNot(_.isZero))
}

object MomentMatrix {

  def matIterator[A](mat: Mat[A]): Iterator[A] = for {
    r <- Iterator.range(0, mat.nRows)
    c <- Iterator.range(0, mat.nCols)
  } yield mat(r, c)

  def apply[
    E <: generic.Evaluator.Aux[M] with Singleton: Witness.Aux,
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](generatingMoments: OrderedSet[M#Monomial], optimize: Boolean = true): MomentMatrix[E, M] = {
    def E: E = valueOf[E]
    def M: M = valueOf[M]
    val size = generatingMoments.length
    val matrixSymmetries = MatrixSymmetries.fromEquivalences(E.equivalences, generatingMoments)
    val moments: Mat[E#EvaluatedMonomial] =
      if (matrixSymmetries.grp.isTrivial && !optimize)
        Mat.tabulate(size, size) { (r, c) => E(generatingMoments(r).adjoint * generatingMoments(c))}
      else
        Mat.fromMutable[E#EvaluatedMonomial](size, size, E.zero) { mat =>
          val conf = matrixSymmetries.configuration
          cforRange(0 until conf.nOrbits) { o =>
            val ptr: symmetries.Ptr[conf.type] = conf.orbitStart(o)
            val r = ptr.row
            val c = ptr.col
            val v = E(generatingMoments(ptr.row).adjoint * generatingMoments(ptr.col))
            mat(r, c) := v
            @tailrec def rec(p: symmetries.Ptr[conf.type]): Unit =
              if (!p.isEmpty) {
                mat(p.row, p.col) := v <* p.phase
                rec(p.next)
              }
            rec(ptr.next)
          }
        }
    new MomentMatrix[E, M](generatingMoments, moments, matrixSymmetries.onPermutationGroup)
  }

}
