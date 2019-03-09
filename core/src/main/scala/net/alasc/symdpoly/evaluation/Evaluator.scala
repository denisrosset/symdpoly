package net.alasc.symdpoly
package evaluation

import scala.annotation.tailrec

import cats.{Contravariant, Invariant}
import shapeless.Witness
import spire.algebra.{Action, Eq, Order, VectorSpace}
import spire.syntax.action._

import cyclo.Cyclo

import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.generic
import cats.instances.order.catsContravariantMonoidalForOrder
import cats.instances.eq.catsContravariantMonoidalForEq
import spire.math.Rational

import net.alasc.finite.Grp
import spire.util.Opt
import spire.syntax.std.seq._

import syntax.all._
import instances.all._

/** Describes a quotient vector space defined on a polynomial ring.
  *
  * The quotient space is defined through equivalence relations that do not necessarily
  * preserve the polynomial structure.
  *
  */
abstract class Evaluator[M <: generic.MonoidDef with Singleton: Witness.Aux] { self =>

  def equivalences: Seq[Equivalence[M]]

  def M: M = valueOf[M]
  val witness: Witness.Aux[self.type] = Witness.mkWitness(self)

  type ScratchPad
  def makeScratchPad: ScratchPad

  // optimization: set to true if apply(a) == apply(a.adjoint)
  def isSelfAdjoint: Boolean = ??? // TODO equivalences.exists(e => e.isInstanceOf[AdjointEquivalence[_]] || e.isInstanceOf[AdjointFreeBasedEquivalence[_, _]])

  //region Evaluated monomials

  type EvaluatedMonomial = EvaluatedMono[self.type, M]

  def apply(mono: M#Monomial): EvaluatedMono[self.type, M] = apply(mono, makeScratchPad)
  def apply(mono: M#Monomial, pad: ScratchPad): EvaluatedMono[self.type, M]

  //endregion

  //region Evaluated polynomials

  type EvaluatedPolynomial = EvaluatedPoly[self.type, M]

  def apply(poly: M#Polynomial)(implicit d: DummyImplicit): EvaluatedPoly[self.type, M] = apply(poly, makeScratchPad)

  def apply(poly: M#Polynomial, pad: ScratchPad)(implicit d: DummyImplicit): EvaluatedPoly[self.type, M]

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(i: Int): EvaluatedPolynomial = apply(M.constant(i))

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(r: Rational): EvaluatedPolynomial = apply(M.constant(r))

  /** Construct a constant evaluated polynomial from the given constant. */
  def constant(c: Cyclo): EvaluatedPolynomial = apply(M.constant(c))

  //endregion

  type Permutation = M#Permutation

  //region Typeclasses

  val evaluatedMonoZero: EvaluatedMonomial = new EvaluatedMono[self.type, M](M.monoMultiplicativeBinoid.zero)
  val evaluatedMonoOrder: Order[EvaluatedMonomial] = Contravariant[Order].contramap(M.monoOrder)(em => em.normalForm)
  val evaluatedMonoPhased: Phased[EvaluatedMonomial] = Invariant[Phased].imap(M.monoPhased)(apply(_, makeScratchPad))(_.normalForm)
  val evaluatedPolyEq: Eq[EvaluatedPolynomial] = Contravariant[Eq].contramap(M.polyEq)(ep => ep.normalForm)
  val evaluatedPolyVectorSpace: VectorSpace[EvaluatedPolynomial, Cyclo] = Invariant[Lambda[V => VectorSpace[V, Cyclo]]].imap(M.polyAssociativeAlgebra)(apply(_, makeScratchPad))(_.normalForm)

  def evaluatedMonoPermutationAction: Action[EvaluatedMonomial, Permutation]

  //endregion
}

case class GenericEvaluator[M <: generic.MonoidDef with Singleton: Witness.Aux](equivalences: Seq[Equivalence[M]]) extends Evaluator[M] { self =>

  val evaluatedMonoPermutationAction: Action[EvaluatedMonomial, Permutation] = {
    val action: Action[M#Monomial, Permutation] = (M: M).permutationMonoAction
    Invariant[Lambda[P => Action[P, Permutation]]].imap[M#Monomial, EvaluatedMono[self.type, M]](action)(m => apply(m))(_.normalForm)
  }

  type ScratchPad = Unit
  def makeScratchPad: Unit = ()

  def apply(mono: M#Monomial, pad: ScratchPad): EvaluatedMono[self.type, M] = {
    val candidates = equivalences.foldLeft(Set(mono)) { case (set, equivalence) => set.flatMap(m => equivalence(m)) }
    val grouped = candidates.groupBy(M.monoPhased.phaseCanonical)
    if (grouped.values.exists(_.size > 1))
      new EvaluatedMono[self.type, M](M.monoMultiplicativeBinoid.zero)
    else
      new EvaluatedMono[self.type, M](candidates.qmin(M.monoOrder))
  }

  def apply(poly: M#Polynomial, pad: ScratchPad)(implicit d: DummyImplicit): EvaluatedPoly[self.type, M] = {
    @tailrec def iter(i: Int, acc: M#Polynomial): M#Polynomial =
      if (i == poly.nTerms) acc else {
        val newTerm = M.polyAssociativeAlgebra.timesl(poly.coeff(i), M.monomialToPolynomial(apply(poly.monomial(i), pad).normalForm))
        iter(i + 1, M.polyAssociativeAlgebra.plus(acc, newTerm))
      }
    new EvaluatedPoly[self.type, M](iter(0, M.polyAssociativeAlgebra.zero))
  }

}
