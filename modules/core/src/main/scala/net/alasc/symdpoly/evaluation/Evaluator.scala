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
import net.alasc.symdpoly.generic.{SingleMoment, LinearMoment}
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
  def symmetryGroup: Grp[Mono#PermutationType]
  lazy val symmetryGroupDecomposition: GrpDecomposition[Mono#PermutationType] = GrpDecomposition(symmetryGroup)

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
  def compatibleSubgroup(grp: Grp[Mono#PermutationType]): Grp[Mono#PermutationType] = grp intersect (symmetryGroup union equivalence.compatibleSubgroup(grp))

  //region Evaluated monomials

  type SingleMomentType = SingleMoment[self.type, Mono]

  lazy val zero: SingleMomentType = apply(M.zero)

  lazy val one: SingleMomentType = apply(M.one)

  def fromNormalForm(normalForm: Mono#MonoType): SingleMoment[self.type, Mono] = new SingleMoment[self.type, Mono](normalForm)

  def apply(mono: Mono#MonoType): SingleMoment[self.type, Mono] = {
    val equivalentUnderSymmetry: Set[Mono#MonoType] = symmetryGroupDecomposition.transversals.foldLeft(Set(mono)) {
      case (set, transversal) => for ( m <- set ; g <- transversal ) yield m <|+| g
    }
    val candidates = equivalentUnderSymmetry.flatMap( equivalence.apply(_: Mono#MonoType) )
    val canonicalCandidates = candidates.map(M.monoPhased.phaseCanonical)
    if (canonicalCandidates.size != candidates.size)
      new SingleMoment[self.type, Mono](M.monoMultiplicativeBinoid.zero)
    else
      new SingleMoment[self.type, Mono](candidates.qmin(M.monoOrder))
  }

  //endregion

  //region Evaluated polynomials

  type LinearMomentType = LinearMoment[self.type, Mono]

  def apply(poly: Mono#PolyType)(implicit d: DummyImplicit): LinearMoment[self.type, Mono] = {
    @tailrec def iter(i: Int, acc: Mono#PolyType): Mono#PolyType =
      if (i == poly.nTerms) acc else {
        val newTerm = M.polyAssociativeAlgebra.timesl(poly.coeff(i), M.monomialToPolynomial(apply(poly.monomial(i)).normalForm))
        iter(i + 1, M.polyAssociativeAlgebra.plus(acc, newTerm))
      }
    new LinearMoment[self.type, Mono](iter(0, M.polyAssociativeAlgebra.zero))
  }

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(i: Int): LinearMomentType = apply(M.constant(i))

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(r: Rational): LinearMomentType = apply(M.constant(r))

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(c: Cyclo): LinearMomentType = apply(M.constant(c))

  //endregion

  //region Typeclasses

  lazy val evaluatedMonoZero: SingleMomentType = new SingleMoment[self.type, Mono](M.monoMultiplicativeBinoid.zero)
  lazy val evaluatedMonoOrder: Order[SingleMomentType] = Contravariant[Order].contramap(M.monoOrder)(em => em.normalForm)
  lazy val evaluatedMonoInvolution: Involution[SingleMomentType] = Invariant[Involution].imap(M.monoInvolution)(apply)(_.normalForm)
  lazy val evaluatedMonoPhased: Phased[SingleMomentType] = Invariant[Phased].imap(M.monoPhased)(fromNormalForm)(_.normalForm)
  lazy val evaluatedMonoClassTag: ClassTag[SingleMomentType] = implicitly

  lazy val evaluatedPolyInvolution: Involution[LinearMomentType] = Invariant[Involution].imap(M.polyInvolution)(apply)(_.normalForm)
  lazy val evaluatedPolyEq: Eq[LinearMomentType] = Contravariant[Eq].contramap(M.polyEq)(ep => ep.normalForm)
  lazy val evaluatedPolyVectorSpace: VectorSpace[LinearMomentType, Cyclo] = Invariant[Lambda[V => VectorSpace[V, Cyclo]]].imap(M.polyAssociativeAlgebra)(apply)(_.normalForm)
  lazy val evaluatedPolyClassTag: ClassTag[LinearMomentType] = implicitly

  lazy val evaluatedMonoPermutationAction: Action[SingleMomentType, Mono#PermutationType] = new Action[SingleMomentType, Mono#PermutationType] {
    def actr(p: SingleMomentType, g: Mono#PermutationType): SingleMomentType = apply(M.permutationMonoAction.actr(p.normalForm, g))
    def actl(g: Mono#PermutationType, p: SingleMomentType): SingleMomentType = apply(M.permutationMonoAction.actl(g, p.normalForm))
  }

  //endregion
}

object Evaluator {

  type Aux[M <: generic.MonoidDef with Singleton] = Evaluator { type Mono = M }

  implicit def evaluatedPolyVectorSpace[E <: Evaluator with Singleton: Witness.Aux]: VectorSpace[E#LinearMomentType, Cyclo] = valueOf[E].evaluatedPolyVectorSpace
  implicit def evaluatedPolyInvolution[E <: Evaluator with Singleton: Witness.Aux]: Involution[E#LinearMomentType] = valueOf[E].evaluatedPolyInvolution
  implicit def evaluatedPolyEq[E <: Evaluator with Singleton: Witness.Aux]: Eq[E#LinearMomentType] = valueOf[E].evaluatedPolyEq
  implicit def evaluatedPolyClassTag[E <: Evaluator with Singleton: Witness.Aux]: ClassTag[E#LinearMomentType] = valueOf[E].evaluatedPolyClassTag

}
