package net.alasc.symdpoly
package generic

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
class Evaluator[M <: generic.MonoidDef with Singleton: Witness.Aux](val equivalences: Seq[Equivalence[M]]) { self =>

  def M: M = valueOf[M]
  val witness: Witness.Aux[self.type] = Witness.mkWitness(self)

  // optimization: set to true if apply(a) == apply(a.adjoint)
  def isSelfAdjoint: Boolean = ??? // TODO equivalences.exists(e => e.isInstanceOf[AdjointEquivalence[_]] || e.isInstanceOf[AdjointFreeBasedEquivalence[_, _]])

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

  type Permutation = generic.EvaluatedPermutation[self.type, M]

  //region Typeclasses

  val evaluatedMonoZero: EvaluatedMonomial = new EvaluatedMono[self.type, M](M.monoMultiplicativeBinoid.zero)
  val evaluatedMonoOrder: Order[EvaluatedMonomial] = Contravariant[Order].contramap(M.monoOrder)(em => em.normalForm)
  val evaluatedMonoPhased: Phased[EvaluatedMonomial] = Invariant[Phased].imap(M.monoPhased)(apply)(_.normalForm)
  val evaluatedPolyEq: Eq[EvaluatedPolynomial] = Contravariant[Eq].contramap(M.polyEq)(ep => ep.normalForm)
  val evaluatedPolyVectorSpace: VectorSpace[EvaluatedPolynomial, Cyclo] = Invariant[Lambda[V => VectorSpace[V, Cyclo]]].imap(M.polyAssociativeAlgebra)(apply)(_.normalForm)

  val evaluatedMonoPermutationAction: Action[EvaluatedMonomial, Permutation] = new Action[EvaluatedMonomial, Permutation] {
    def actr(p: EvaluatedMonomial, g: Permutation): EvaluatedMonomial = apply(M.permutationMonoAction.actr(p.normalForm, g.permutation))
    def actl(g: Permutation, p: EvaluatedMonomial): EvaluatedMonomial = apply(M.permutationMonoAction.actl(g.permutation, p.normalForm))
  }

  //endregion
}
