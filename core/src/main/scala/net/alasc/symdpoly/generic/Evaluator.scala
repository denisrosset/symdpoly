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
import net.alasc.perms.default._

/** Describes a quotient vector space defined on a polynomial ring.
  *
  * The quotient space is defined through equivalence relations that do not necessarily
  * preserve the polynomial structure.
  *
  */
abstract class Evaluator { self =>

  def equivalences: Seq[Equivalence[Mono]]
  type Mono <: generic.MonoidDef with Singleton
  implicit val witness: Witness.Aux[self.type] = Witness.mkWitness(self)
  implicit def witnessMono: Witness.Aux[Mono]
  def M: Mono = valueOf[Mono]

  def isSelfAdjoint: Boolean = equivalences.exists {
    case _: generic.AdjointEquivalence[_] => true
    case _: freebased.AdjointEquivalence[_, _] => true
    case _ => false
  }

  //region Evaluated monomials

  type EvaluatedMonomial = EvaluatedMono[self.type, Mono]

  lazy val zero: EvaluatedMonomial = apply(M.zero)

  lazy val one: EvaluatedMonomial = apply(M.one)

  def fromNormalForm(normalForm: Mono#Monomial): EvaluatedMono[self.type, Mono] = new EvaluatedMono[self.type, Mono](normalForm)

  def apply(mono: Mono#Monomial): EvaluatedMono[self.type, Mono] = {
    val candidates = equivalences.foldLeft(Set(mono)) { case (set, equivalence) => set.flatMap(m => equivalence(m)) }
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

  type Permutation = EvaluatedPermutation[self.type, Mono]

  /** Returns the permutation in the evaluator corresponding ot the given permutation on the monoid. */
  def permutationNC(p: Mono#Permutation): EvaluatedPermutation[self.type, Mono] = new EvaluatedPermutation[self.type, Mono](p)

  /** Symmetry group compatible with this evaluator. */
  def symmetryGroup: Grp[Permutation] = groupInEvaluator(M.symmetryGroup)

  /** Returns the subgroup of a group of permutations on the monoid that is compatible with the evaluator. */
  def groupInEvaluator(grp: Grp[Mono#Permutation]): Grp[Permutation] =
    groupInEvaluatorNC(equivalences.foldLeft(grp) {
      case (curGrp, equiv) => equiv.groupInEvaluator(curGrp)
    })

  /** Translates a group on the monoid onto a group on the evaluated objects, without checking compatibility. */
  def groupInEvaluatorNC(grp: Grp[Mono#Permutation]): Grp[Permutation] =
    Grp.fromGeneratorsAndOrder(grp.generators.map(permutationNC), grp.order)(permRepGrpBuilder(Evaluator.permutationClassTag, Evaluator.permutationEq, Evaluator.permutationGroup, Evaluator.permutationFaithfulPermutationActionBuilder))

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

  val permutationGroup: Group[Permutation] = Invariant[Group].imap(M.permutationGroup)(mp => new EvaluatedPermutation[self.type, Mono](mp))(_.permutation)
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

  implicit def permutationGroup[E <: Evaluator with Singleton: Witness.Aux]: Group[E#Permutation] = valueOf[E].permutationGroup
  implicit def permutationEq[E <: Evaluator with Singleton: Witness.Aux]: Eq[E#Permutation] = valueOf[E].permutationEq
  implicit def permutationFaithfulPermutationActionBuilder[E <: Evaluator with Singleton: Witness.Aux]: FaithfulPermutationActionBuilder[E#Permutation] = valueOf[E].permutationFaithfulPermutationActionBuilder
  implicit def permutationClassTag[E <: Evaluator with Singleton: Witness.Aux]: ClassTag[E#Permutation] = valueOf[E].permutationClassTag

  implicit def permutationMonoAction[E <: Evaluator with Singleton: Witness.Aux]: Action[E#EvaluatedMonomial, E#Permutation] = valueOf[E].evaluatedMonoPermutationAction

}
