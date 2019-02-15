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

import net.alasc.finite.Grp
import net.alasc.symdpoly.algebra.Instances._
import spire.util.Opt
import spire.syntax.std.seq._

import net.alasc.symdpoly.algebra.Phased.syntax._
import net.alasc.symdpoly.evaluation.Equivalence.{CyclicEquivalence, FullAdjointEquivalence, TransposeEquivalence}

abstract class Evaluator2[M <: generic.MonoidDef with Singleton: Witness.Aux](val equivalences: Seq[Equivalence2[M]]) { self =>

  type E <: Evaluator2[M]

  def M: M = valueOf[M]
  val witness: Witness.Aux[self.type] = Witness.mkWitness(self)

  type ScratchPad
  def makeScratchPad: ScratchPad

  // optimization: set to true if apply(a) == apply(a.adjoint)
  def isSelfAdjoint: Boolean = equivalences.exists(e => e.isInstanceOf[AdjointEquivalence2[_]] || e.isInstanceOf[AdjointFreeBasedEquivalence2[_, _]])

  def apply(mono: M#Monomial): EvaluatedMono2[self.type, M] = apply(mono, makeScratchPad)
  def apply(mono: M#Monomial, pad: ScratchPad): EvaluatedMono2[self.type, M]

  def apply(poly: M#Polynomial)(implicit d: DummyImplicit): EvaluatedPoly2[self.type, M] = apply(poly, makeScratchPad)
  def apply(poly: M#Polynomial, pad: ScratchPad)(implicit d: DummyImplicit): EvaluatedPoly2[self.type, M]

  type EvaluatedMonomial = EvaluatedMono2[self.type, M]
  type EvaluatedPolynomial = EvaluatedPoly2[self.type, M]
  type Permutation = M#Permutation

  // typeclasses

  val evaluatedMonoZero: EvaluatedMonomial = new EvaluatedMono2[self.type, M](M.monoMultiplicativeBinoid.zero)
  val evaluatedMonoOrder: Order[EvaluatedMonomial] = Contravariant[Order].contramap(M.monoOrder)(em => em.normalForm)
  val evaluatedMonoPhased: Phased[EvaluatedMonomial] = Invariant[Phased].imap(M.monoPhased)(apply(_, makeScratchPad))(_.normalForm)
  val evaluatedPolyEq: Eq[EvaluatedPolynomial] = Contravariant[Eq].contramap(M.polyEq)(ep => ep.normalForm)
  val evaluatedPolyVectorSpace: VectorSpace[EvaluatedPolynomial, Cyclo] = Invariant[Lambda[V => VectorSpace[V, Cyclo]]].imap(M.polyAssociativeAlgebra)(apply(_, makeScratchPad))(_.normalForm)

  def evaluatedMonoPermutationAction: Action[EvaluatedMonomial, Permutation]

  def :+(e: Equivalence2[M]): E

  def real: E

  def symmetric[G](grp: Grp[G])(implicit action: Action[M#Monomial, G]): E

}

final class GenericEvaluator2[M <: generic.MonoidDef with Singleton: Witness.Aux](equivalences: Seq[Equivalence2[M]]) extends Evaluator2[M](equivalences) { self =>
  type E = GenericEvaluator2[M]

  val evaluatedMonoPermutationAction: Action[EvaluatedMonomial, Permutation] = {
    val action: Action[M#Monomial, Permutation] = (M: M).permutationMonoAction
    Invariant[Lambda[P => Action[P, Permutation]]].imap[M#Monomial, EvaluatedMono2[self.type, M]](action)(m => apply(m))(_.normalForm)
  }


  type ScratchPad = Unit
  def makeScratchPad: Unit = ()

  def apply(mono: M#Monomial, pad: ScratchPad): EvaluatedMono2[self.type, M] = {
    val candidates = equivalences.foldLeft(Set(mono)) { case (set, equivalence) => set.flatMap(m => equivalence(m)) }
    val grouped = candidates.groupBy(M.monoPhased.phaseCanonical)
    if (grouped.values.exists(_.size > 1))
      new EvaluatedMono2[self.type, M](M.monoMultiplicativeBinoid.zero)
    else
      new EvaluatedMono2[self.type, M](candidates.qmin(M.monoOrder))
  }

  def apply(poly: M#Polynomial, pad: ScratchPad)(implicit d: DummyImplicit): EvaluatedPoly2[self.type, M] = {
    @tailrec def iter(i: Int, acc: M#Polynomial): M#Polynomial =
      if (i == poly.nTerms) acc else {
        val newTerm = M.polyAssociativeAlgebra.timesl(poly.coeff(i), M.monomialToPolynomial(apply(poly.monomial(i), pad).normalForm))
        iter(i + 1, M.polyAssociativeAlgebra.plus(acc, newTerm))
      }
    new EvaluatedPoly2[self.type, M](iter(0, M.polyAssociativeAlgebra.zero))
  }

  def :+(e: Equivalence2[M]): GenericEvaluator2[M] = new GenericEvaluator2[M](equivalences :+ e)

  def real: GenericEvaluator2[M] = self :+ new AdjointEquivalence2[M]

  def symmetric[G](grp: Grp[G])(implicit action: Action[M#Monomial, G]): GenericEvaluator2[M] = self :+ new SymmetryEquivalence2(grp)

}
