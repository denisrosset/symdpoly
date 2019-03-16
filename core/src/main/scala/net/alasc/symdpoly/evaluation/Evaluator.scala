package net.alasc.symdpoly
package evaluation

import scala.annotation.tailrec

import cats.instances.eq.catsContravariantMonoidalForEq
import cats.instances.order.catsContravariantMonoidalForOrder
import cats.{Contravariant, Invariant}
import shapeless.Witness
import spire.ClassTag
import spire.algebra._
import spire.syntax.action._
import spire.math.Rational
import spire.syntax.std.seq._
import instances.invariant._
import cyclo.Cyclo

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.perms.default._
import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.generic.{EvaluatedMono, EvaluatedPoly}
import net.alasc.symdpoly.math.GrpDecomposition
import net.alasc.symdpoly.{freebased, generic, valueOf}

/** Describes a quotient vector space defined on a polynomial ring.
  *
  * The quotient space is defined through equivalence relations that do not necessarily
  * preserve the polynomial structure.
  *
  */
abstract class Evaluator { self =>

  //region Members to implement in concrete instances

  def equivalence: Equivalence[Mono]
  def symmetryGroup: Grp[Mono#Permutation]
  lazy val symmetryGroupDecomposition: GrpDecomposition[Mono#Permutation] = GrpDecomposition(symmetryGroup)

  type Mono <: generic.MonoidDef with Singleton

  //endregion

  implicit def witnessMono: Witness.Aux[Mono]
  implicit val witness: Witness.Aux[self.type] = Witness.mkWitness(self)
  def M: Mono = valueOf[Mono]

  def isSelfAdjoint: Boolean = equivalence.isSelfAdjoint

  /** Returns the subgroup of grp that is compatible with this evaluator, in the sense that
    * for all monomials m1 and m2 of type Mono#Monomial, and all g of type M#Permutation, we have
    * L(m1) == L(m2) if and only if L(m1 <|+| g) == L(m2 <|+| g)
    */
  def compatibleSubgroup(grp: Grp[Mono#Permutation]): Grp[Mono#Permutation] = grp intersect (symmetryGroup union equivalence.compatibleSubgroup(grp))

  //region Evaluated monomials

  type EvaluatedMonomial = EvaluatedMono[self.type, Mono]

  lazy val zero: EvaluatedMonomial = apply(M.zero)

  lazy val one: EvaluatedMonomial = apply(M.one)

  def fromNormalForm(normalForm: Mono#Monomial): EvaluatedMono[self.type, Mono] = new EvaluatedMono[self.type, Mono](normalForm)

  def apply(mono: Mono#Monomial): EvaluatedMono[self.type, Mono] = {
    val equivalentUnderSymmetry = symmetryGroupDecomposition.transversals.foldLeft(Set(mono)) {
      case (set, transversal) => for ( m <- set ; g <- transversal ) yield m <|+| g
    }
    val candidates = equivalentUnderSymmetry.flatMap( equivalence.apply(_) )
    val canonicalCandidates = candidates.map(_.phaseCanonical)
    if (canonicalCandidates.size != candidates.size)
      new EvaluatedMono[self.type, Mono](M.monoMultiplicativeBinoid.zero)
    else
      new EvaluatedMono[self.type, Mono](candidates.qmin(M.monoOrder))
  }

  //endregion

  //region Evaluated polynomials

  type EvaluatedPolynomial = EvaluatedPoly[self.type, Mono]

  def apply(poly: Mono#Polynomial)(implicit d: DummyImplicit): EvaluatedPoly[self.type, Mono] = {
    @tailrec def iter(i: Int, acc: Mono#Polynomial): Mono#Polynomial =
      if (i == poly.nTerms) acc else {
        val newTerm = M.polyAssociativeAlgebra.timesl(poly.coeff(i), M.monomialToPolynomial(apply(poly.monomial(i)).normalForm))
        iter(i + 1, M.polyAssociativeAlgebra.plus(acc, newTerm))
      }
    new EvaluatedPoly[self.type, Mono](iter(0, M.polyAssociativeAlgebra.zero))
  }

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(i: Int): EvaluatedPolynomial = apply(M.constant(i))

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(r: Rational): EvaluatedPolynomial = apply(M.constant(r))

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(c: Cyclo): EvaluatedPolynomial = apply(M.constant(c))

  //endregion

  //region Typeclasses

  val evaluatedMonoZero: EvaluatedMonomial = new EvaluatedMono[self.type, Mono](M.monoMultiplicativeBinoid.zero)
  val evaluatedMonoOrder: Order[EvaluatedMonomial] = Contravariant[Order].contramap(M.monoOrder)(em => em.normalForm)
  val evaluatedMonoInvolution: Involution[EvaluatedMonomial] = Invariant[Involution].imap(M.monoInvolution)(apply)(_.normalForm)
  val evaluatedMonoPhased: Phased[EvaluatedMonomial] = Invariant[Phased].imap(M.monoPhased)(fromNormalForm)(_.normalForm)
  val evaluatedMonoClassTag: ClassTag[EvaluatedMonomial] = implicitly

  val evaluatedPolyInvolution: Involution[EvaluatedPolynomial] = Invariant[Involution].imap(M.polyInvolution)(apply)(_.normalForm)
  val evaluatedPolyEq: Eq[EvaluatedPolynomial] = Contravariant[Eq].contramap(M.polyEq)(ep => ep.normalForm)
  val evaluatedPolyVectorSpace: VectorSpace[EvaluatedPolynomial, Cyclo] = Invariant[Lambda[V => VectorSpace[V, Cyclo]]].imap(M.polyAssociativeAlgebra)(apply)(_.normalForm)
  val evaluatedPolyClassTag: ClassTag[EvaluatedPolynomial] = implicitly

  val evaluatedMonoPermutationAction: Action[EvaluatedMonomial, Mono#Permutation] = new Action[EvaluatedMonomial, Mono#Permutation] {
    def actr(p: EvaluatedMonomial, g: Mono#Permutation): EvaluatedMonomial = apply(M.permutationMonoAction.actr(p.normalForm, g))
    def actl(g: Mono#Permutation, p: EvaluatedMonomial): EvaluatedMonomial = apply(M.permutationMonoAction.actl(g, p.normalForm))
  }

  //endregion
}

object Evaluator {
  
  type Aux[M <: generic.MonoidDef with Singleton] = Evaluator { type Mono = M }

  /*
  implicit def evaluatedMonoInvolution[E <: Evaluator with Singleton: Witness.Aux]: Involution[E#EvaluatedMonomial] = valueOf[E].evaluatedMonoInvolution
  implicit def evaluatedMonoOrder[E <: Evaluator with Singleton: Witness.Aux]: Order[E#EvaluatedMonomial] = valueOf[E].evaluatedMonoOrder
  implicit def evaluatedMonoPhased[E <: Evaluator with Singleton: Witness.Aux]: Phased[E#EvaluatedMonomial] = valueOf[E].evaluatedMonoPhased
  implicit def evaluatedMonoClassTag[E <: Evaluator with Singleton: Witness.Aux]: ClassTag[E#EvaluatedMonomial] = valueOf[E].evaluatedMonoClassTag
   */
  implicit def evaluatedPolyVectorSpace[E <: Evaluator with Singleton: Witness.Aux]: VectorSpace[E#EvaluatedPolynomial, Cyclo] = valueOf[E].evaluatedPolyVectorSpace
  implicit def evaluatedPolyInvolution[E <: Evaluator with Singleton: Witness.Aux]: Involution[E#EvaluatedPolynomial] = valueOf[E].evaluatedPolyInvolution
  implicit def evaluatedPolyEq[E <: Evaluator with Singleton: Witness.Aux]: Eq[E#EvaluatedPolynomial] = valueOf[E].evaluatedPolyEq
  implicit def evaluatedPolyClassTag[E <: Evaluator with Singleton: Witness.Aux]: ClassTag[E#EvaluatedPolynomial] = valueOf[E].evaluatedPolyClassTag

}
