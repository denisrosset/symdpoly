package net.alasc.symdpoly

import scala.collection.immutable.HashSet

import shapeless.Witness

import scalin.immutable.Mat

import syntax.phased._
import spire.syntax.eq._
import spire.syntax.involution._
import spire.syntax.field._
import scala.collection.compat._

import net.alasc.symdpoly.util.OrderedSet
import scalin.immutable.dense._

import net.alasc.perms.Perm
import net.alasc.symdpoly.sdp.{BasisTerm, Block, BlockElement}

/** Localizing matrix
  *
  * @param polynomial Self-adjoint polynomial being localized
  * @param generatingMoments Generating moments of this localizing matrix
  * @param mat Matrix of evaluated polynomials, such that mat(r, c) = E(generatingMoments(r).adjoint * polynomial * generatingMoments(c))
  * @tparam E Evaluator
  * @tparam M Monomial monoid
  */
class LocalizingMatrix[
  E <: evaluation.Evaluator.Aux[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](val polynomial: M#Polynomial, val generatingMoments: OrderedSet[M#Monomial], val mat: Mat[E#EvaluatedPolynomial]) {
  def E: E = valueOf[E]
  def M: M = valueOf[M]

  /** The matrix of moments has shape size x size */
  def size: Int =  generatingMoments.length

  def allMoments: HashSet[E#EvaluatedMonomial] =
    MomentMatrix.matIterator(mat).flatMap(p => Iterator.tabulate(p.nTerms)(i => p.monomial(i).phaseCanonical).filterNot(_.isZero)).to(HashSet)

  def expandIn(relaxation: Relaxation[E, M]): (Block, Boolean) = {
    val nonZeroElements: Seq[BlockElement] = for {
      r <- 0 until mat.nRows
      c <- 0 until mat.nCols
      poly = mat(r, c)
      i <- 0 until poly.nTerms
      mono = poly.monomial(i)
      coeff = poly.coeff(i)
      BasisTerm(dualIndex, realPart, complexPart) <- mono.expandIn(relaxation, coeff)
    } yield BlockElement(dualIndex, r, c, realPart, complexPart)
    val hasComplexEncoding = nonZeroElements.exists(_.complexPart != 0)
    (Block(mat.nRows, relaxation.allMoments.length, nonZeroElements), hasComplexEncoding)
  }

}

object LocalizingMatrix {

  def apply[
    E <: evaluation.Evaluator.Aux[M] with Singleton: Witness.Aux,
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](polynomial: M#Polynomial, generatingMoments: OrderedSet[M#Monomial]): LocalizingMatrix[E, M] = {
    def E: E = valueOf[E]
    def M: M = valueOf[M]
    require(polynomial.adjoint === polynomial)
    val size = generatingMoments.length
    val moments: Mat[E#EvaluatedPolynomial] =
        Mat.tabulate(size, size) { (r, c) => E(generatingMoments(r).adjoint.toPoly * polynomial * generatingMoments(c).toPoly) }
    new LocalizingMatrix[E, M](polynomial, generatingMoments, moments)
  }

}
