package net.alasc.symdpoly
package evaluation

import scala.annotation.tailrec

import cats.Invariant
import shapeless.Witness
import spire.algebra.Action
import spire.syntax.cfor._
import spire.syntax.std.seq._
import cats.syntax.invariant._
import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.TodoEquivalence.{CyclicEquivalence, TransposeEquivalence}
import net.alasc.symdpoly.generic.{FreeBasedMono, FreeBasedPermutation, FreeBasedPermutationMonoAction}
import net.alasc.symdpoly.math.GrpDecomposition
import syntax.all._
import instances.all._

final class FreeBasedEvaluator[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton: Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](equivalences: Seq[Equivalence[M]]) extends Evaluator[M](equivalences) { self =>

  def F: F = M.Free
  implicit def witnessF: Witness.Aux[F] = F.witness

  type E = FreeBasedEvaluator[M, F]

  type ScratchPad = FreeScratchPad[F]
  def makeScratchPad: FreeScratchPad[F] = FreeScratchPad.apply[F]

  val evaluatedMonoPermutationAction: Action[EvaluatedMonomial, Permutation] = {
    val action: Action[FreeBasedMono[M, F], Permutation] = (M: M).permutationMonoAction
    Invariant[Lambda[P => Action[P, FreeBasedPermutation[M, F]]]].imap[M#Monomial, EvaluatedMono[self.type, M]](action)(m => apply(m))(_.normalForm)
  }

  def apply(mono: FreeBasedMono[M, F], pad: FreeScratchPad[F]): EvaluatedMono[this.type, M] = {
    val word = mono.data.mutableCopy()
    reduce(word, pad)
    new EvaluatedMono[this.type, M](new FreeBasedMono[M, F](word.setImmutable()))
  }

  override def apply(poly: Poly[M, F], pad: FreeScratchPad[F])(implicit d: DummyImplicit): EvaluatedPoly[this.type, M] = {
    val resPoly = free.MutablePoly.empty[F](poly.nTerms)
    cforRange(0 until poly.nTerms) { i =>
      val word = poly.monomialNormalForm(i).mutableCopy()
      reduce(word, pad)
      val coeff = poly.coeff(i) * word.phase.toCyclo
      word.setPhase(Phase.one)
      resPoly.add(word.setImmutable(), coeff)
    }
    new EvaluatedPoly[this.type, M](resPoly.immutableCopy)
  }

  def applyAndCheckForZero(pad: FreeScratchPad[F]): Boolean =
    equivalences.exists { // bail out when any equivalence has detected that the monomial is zero
      case fbe: FreeBasedEquivalence[M, F] => fbe.expandAndCheckForZero(pad)
      case e =>
        val n = pad.n
        val monos = Array.tabulate(n)(i => pad.scratch(i).immutableCopy)
        var ind = 0
        cforRange(0 until n) { i =>
          val mono = new FreeBasedMono[M, F](monos(i))
          e(mono).foreach { newMono =>
            pad.scratch(ind).setToContentOf(newMono.data)
            ind += 1
          }
        }
        pad.n = ind
        pad.removeDuplicatesAndCheckForZero()
    }

  def reduce(word: free.MutableWord[F], pad: FreeScratchPad[F]): free.MutableWord[F] = {
    M.inPlaceNormalForm(word)
    if (word.isZero) word else {
      pad.resetWithCopyOf(word)
      if (applyAndCheckForZero(pad)) word.setToZero()
      else word.setToContentOf(pad.array(0))
    }
  }


  def :+(e: Equivalence[M]): FreeBasedEvaluator[M, F] = new FreeBasedEvaluator[M, F](equivalences :+ e)

  def real: FreeBasedEvaluator[M, F]  = self :+ new AdjointFreeBasedEquivalence[M, F]

  def symmetric[G](grp: Grp[G])(implicit action: Action[FreeBasedMono[M, F], G]): FreeBasedEvaluator[M, F]  = {
    val e = action match {
      case fbpma: FreeBasedPermutationMonoAction[M, F] =>
        val grp1 = grp.asInstanceOf[Grp[FreeBasedPermutation[M, F]]]
        new SymmetryFreeBasedEquivalence[M, F](grp1)
      case _ =>
        new SymmetryEquivalence[M, G](grp)
    }
    self :+ e
  }

  def cyclic: Evaluator[M] = self :+ new LiftedFreeBasedEquivalence[M, F](new CyclicEquivalence[F](x => true))

  def cyclic(predicate: OpPredicate[F]): Evaluator[M] = self :+ new LiftedFreeBasedEquivalence[M, F](new CyclicEquivalence[F](predicate))

  def transpose(predicate: OpPredicate[F]): Evaluator[M] = self :+ new LiftedFreeBasedEquivalence[M, F](new TransposeEquivalence[F](predicate))

}
