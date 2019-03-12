package net.alasc.symdpoly
package generic

import scala.annotation.tailrec
import cats.{Contravariant, Invariant}
import shapeless.Witness
import spire.algebra.{Action, Eq, Group, Involution, Order, VectorSpace}
import spire.syntax.action._
import cyclo.Cyclo
import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.generic
import cats.instances.order.catsContravariantMonoidalForOrder
import cats.instances.eq.catsContravariantMonoidalForEq
import spire.math.Rational
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import spire.util.Opt
import spire.syntax.std.seq._
import syntax.all._
import instances.all._
import spire.ClassTag

/** Describes a quotient vector space defined on a polynomial ring.
  *
  * The quotient space is defined through equivalence relations that do not necessarily
  * preserve the polynomial structure.
  *
  */
class Evaluator[M <: generic.MonoidDef with Singleton: Witness.Aux](val equivalences: Seq[Equivalence[M]]) { self =>

  def M: M = valueOf[M]
  val witness: Witness.Aux[self.type] = Witness.mkWitness(self)

  //region Evaluated monomials

  type EvaluatedMonomial = EvaluatedMono[self.type, M]

  def apply(mono: M#Monomial): EvaluatedMono[self.type, M] = {
    val candidates = equivalences.foldLeft(Set(mono)) { case (set, equivalence) => set.flatMap(m => equivalence(m)) }
    val grouped = candidates.groupBy(M.monoPhased.phaseCanonical)
    if (grouped.values.exists(_.size > 1))
      new EvaluatedMono[self.type, M](M.monoMultiplicativeBinoid.zero)
    else
      new EvaluatedMono[self.type, M](candidates.qmin(M.monoOrder))
  }

  //endregion

  //region Evaluated polynomials

  type EvaluatedPolynomial = EvaluatedPoly[self.type, M]

  def apply(poly: M#Polynomial)(implicit d: DummyImplicit): EvaluatedPoly[self.type, M] = {
    @tailrec def iter(i: Int, acc: M#Polynomial): M#Polynomial =
      if (i == poly.nTerms) acc else {
        val newTerm = M.polyAssociativeAlgebra.timesl(poly.coeff(i), M.monomialToPolynomial(apply(poly.monomial(i)).normalForm))
        iter(i + 1, M.polyAssociativeAlgebra.plus(acc, newTerm))
      }
    new EvaluatedPoly[self.type, M](iter(0, M.polyAssociativeAlgebra.zero))
  }

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(i: Int): EvaluatedPolynomial = apply(M.constant(i))

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(r: Rational): EvaluatedPolynomial = apply(M.constant(r))

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(c: Cyclo): EvaluatedPolynomial = apply(M.constant(c))

  //endregion

  type Permutation = EvaluatedPermutation[self.type, M]

  /** Returns the permutation in the evaluator corresponding ot the given permutation on the monoid. */
  def permutationNC(p: M#Permutation): EvaluatedPermutation[self.type, M]

  /** Returns the subgroup of a group of permutations on the monoid that is compatible with the evaluator. */
  def groupInEvaluator(grp: Grp[M#Permutation]): Grp[Permutation] = {
    groupInEvaluatorNC(equivalences.foldLeft(grp) {
      case (curGrp, equiv) => equiv.groupInEvaluator(curGrp)
    })

  /** Translates a group on the monoid onto a group on the evaluated objects, without checking compatibility. */
  def groupInEvaluatorNC(grp: Grp[M#Permutation]): Grp[Permutation] =
    Grp.fromGeneratorsAndOrder(grp.generators.map(permutationNC), grp.order)

  //region Typeclasses

  val evaluatedMonoZero: EvaluatedMonomial = new EvaluatedMono[self.type, M](M.monoMultiplicativeBinoid.zero)
  val evaluatedMonoOrder: Order[EvaluatedMonomial] = Contravariant[Order].contramap(M.monoOrder)(em => em.normalForm)
  val evaluatedMonoInvolution: Involution[EvaluatedMonomial] = Invariant[Involution].imap(M.monoInvolution)(apply)(_.normalForm)
  val evaluatedMonoPhased: Phased[EvaluatedMonomial] = Invariant[Phased].imap(M.monoPhased)(apply)(_.normalForm)
  val evaluatedMonoClassTag: ClassTag[EvaluatedMonomial] = implicitly

  val evaluatedPolyInvolution: Involution[EvaluatedPolynomial] = Invariant[Involution].imap(M.polyInvolution)(apply)(_.normalForm)
  val evaluatedPolyEq: Eq[EvaluatedPolynomial] = Contravariant[Eq].contramap(M.polyEq)(ep => ep.normalForm)
  val evaluatedPolyVectorSpace: VectorSpace[EvaluatedPolynomial, Cyclo] = Invariant[Lambda[V => VectorSpace[V, Cyclo]]].imap(M.polyAssociativeAlgebra)(apply)(_.normalForm)
  val evaluatedPolyClassTag: ClassTag[EvaluatedPolynomial] = implicitly

  val permutationGroup: Group[Permutation] = Invariant[Group].imap(M.permutationGroup)(mp => new EvaluatedPermutation[self.type, M](mp))(_.permutation)
  val permutationEq: Eq[Permutation] = Contravariant[Eq].contramap(M.permutationEq)(ep => ep.permutation)
  val permutationFaithfulPermutationActionBuilder: FaithfulPermutationActionBuilder[Permutation]
    = Contravariant[FaithfulPermutationActionBuilder].contramap(M.permutationFaithfulPermutationActionBuilder)(_.permutation)
  val permutationClassTag: ClassTag[Permutation] = implicitly

  val evaluatedMonoPermutationAction: Action[EvaluatedMonomial, Permutation] = new Action[EvaluatedMonomial, Permutation] {
    def actr(p: EvaluatedMonomial, g: Permutation): EvaluatedMonomial = apply(M.permutationMonoAction.actr(p.normalForm, g.permutation))
    def actl(g: Permutation, p: EvaluatedMonomial): EvaluatedMonomial = apply(M.permutationMonoAction.actl(g.permutation, p.normalForm))
  }

  //endregion
}

object Evaluator {

  implicit def evaluatedMonoInvolution[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: Involution[E#EvaluatedMonomial] = valueOf[E].evaluatedMonoInvolution
  implicit def evaluatedMonoOrder[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: Order[E#EvaluatedMonomial] = valueOf[E].evaluatedMonoOrder
  implicit def evaluatedMonoPhased[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: Phased[E#EvaluatedMonomial] = valueOf[E].evaluatedMonoPhased
  implicit def evaluatedMonoClassTag[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: ClassTag[E#EvaluatedMonomial] = valueOf[E].evaluatedMonoClassTag

  implicit def evaluatedPolyVectorSpace[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: VectorSpace[E#EvaluatedPolynomial, Cyclo] = valueOf[E].evaluatedPolyVectorSpace
  implicit def evaluatedPolyInvolution[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: Involution[E#EvaluatedPolynomial] = valueOf[E].evaluatedPolyInvolution
  implicit def evaluatedPolyEq[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: Eq[E#EvaluatedPolynomial] = valueOf[E].evaluatedPolyEq
  implicit def evaluatedPolyClassTag[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: ClassTag[E#EvaluatedPolynomial] = valueOf[E].evaluatedPolyClassTag

  implicit def permutationGroup[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: Group[E#Permutation] = valueOf[E].permutationGroup
  implicit def permutationEq[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: Eq[E#Permutation] = valueOf[E].permutationEq
  implicit def permutationFaithfulPermutationActionBuilder[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: FaithfulPermutationActionBuilder[E#Permutation] = valueOf[E].permutationFaithfulPermutationActionBuilder
  implicit def permutationClassTag[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: ClassTag[E#Permutation] = valueOf[E].permutationClassTag

  implicit def permutationMonoAction[E <: Evaluator[M] with Singleton: Witness.Aux, M <: generic.MonoidDef with Singleton]: Action[E#EvaluatedMonomial, E#Permutation] = valueOf[E].evaluatedMonoPermutationAction

}
