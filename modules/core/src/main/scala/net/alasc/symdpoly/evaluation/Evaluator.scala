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

  type Mono <: generic.MonoDef with Singleton

  implicit def witnessMono: Witness.Aux[Mono]

  /** Current symmetry group */
  def symmetryGroup: Grp[Mono#PermutationType]

  /** Returns the set of monomials equivalent to the given monomial. */
  def apply(mono: Mono#MonoType): SingleMomentType

  protected def buildWithSymmetryGroup(newSymmetryGroup: Grp[Mono#PermutationType]): Evaluator.Aux[Mono]

  /** Derives a new evaluator from this evaluator by adding equivalence under the given symmetry group. */
  def forceSymmetrize(grp: Grp[Mono#PermutationType]): Evaluator.Aux[Mono] =
    buildWithSymmetryGroup(grp union symmetryGroup)

  /** Derives a new evaluator from this evaluator by adding equivalence under the subgroup of the given symmetry group
    * that is compatible with the structure of this evaluator.
    *
    * In clear, given a group G such that x ~ y implies (x <|+| g) ~ (y <|+| g) for all g in G, we find the subgroup H
    * such that L(x) ~ L(y) implies (L(x) <|+| h) ~ (L(y) <|+| h) for h in H.
    */
  def symmetrize(grp: Grp[Mono#PermutationType]): Evaluator.Aux[Mono] =
    forceSymmetrize(compatibleSubgroup(grp))

  /** Returns the group of permutations that is compatible with the evaluator,
    *
    * A group of permutations is deemed compatible if, for any monomials m1 and m2 of type M#Monomial,
    * we have m1 ~ m2 if and only if (m1 <|+| g) ~ (m2 <|+| g) for any element g of the group,
    * where ~ is described by this equivalence relation.
    */
  def compatibleSubgroup(grp: Grp[Mono#PermutationType]): Grp[Mono#PermutationType]

  /** Returns whether m.adjoint is equivalent to m for all m of type M#Monomial. */
  def isReal: Boolean

  //endregion

  implicit val witness: Witness.Aux[self.type] = Witness.mkWitness(self)
  def M: Mono = valueOf[Mono]

  //region Evaluated monomials

  type SingleMomentType = SingleMoment[self.type, Mono]

  lazy val zero: SingleMomentType = new SingleMoment[self.type, Mono](M.zero)

  lazy val one: SingleMomentType = new SingleMoment[self.type, Mono](M.one)

  def fromNormalForm(normalForm: Mono#MonoType): SingleMomentType = new SingleMoment[self.type, Mono](normalForm)

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

  type Aux[M <: generic.MonoDef with Singleton] = Evaluator { type Mono = M }

  implicit def evaluatedPolyVectorSpace[E <: Evaluator with Singleton: Witness.Aux]: VectorSpace[E#LinearMomentType, Cyclo] = valueOf[E].evaluatedPolyVectorSpace
  implicit def evaluatedPolyInvolution[E <: Evaluator with Singleton: Witness.Aux]: Involution[E#LinearMomentType] = valueOf[E].evaluatedPolyInvolution
  implicit def evaluatedPolyEq[E <: Evaluator with Singleton: Witness.Aux]: Eq[E#LinearMomentType] = valueOf[E].evaluatedPolyEq
  implicit def evaluatedPolyClassTag[E <: Evaluator with Singleton: Witness.Aux]: ClassTag[E#LinearMomentType] = valueOf[E].evaluatedPolyClassTag

}
