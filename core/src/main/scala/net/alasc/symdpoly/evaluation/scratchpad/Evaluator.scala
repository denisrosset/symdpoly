package net.alasc.symdpoly
package freebased

import shapeless.Witness


/*
object Evaluator {

  def apply[
    M <: freebased.MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](equivalences: Seq[generic.Equivalence[M]]): generic.Evaluator[M] = generic.GenericEvaluator(equivalences)

}


import cats.Invariant
import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.TodoEquivalence.{CyclicEquivalence, TransposeEquivalence}
import net.alasc.symdpoly.evaluation._
import net.alasc.symdpoly.generic.{FreeBasedMono, FreeBasedPermutation, FreeBasedPoly}
import net.alasc.symdpoly.math.Phase
import net.alasc.symdpoly.{free, generic}
import shapeless.Witness
import spire.algebra.Action
import spire.syntax.cfor._


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

  override def apply(poly: FreeBasedPoly[M, F], pad: FreeScratchPad[F])(implicit d: DummyImplicit): EvaluatedPoly[this.type, M] = {
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
      case fbpma: FreeBasedPermutation.FreeBasedPermutationMonoAction[M, F] =>
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
*/

/*
class GenericFreeBasedEvaluator[M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux, F <: free.MonoidDef.Aux[F] with Singleton: Witness.Aux](equivalences: Seq[Equivalence[M]]) extends GenericEvaluator[M](equivalences) { self =>
  type E = GenericFreeBasedEvaluator[M, F]
  def cyclic: Evaluator[M] = this :+ new LiftedFreeBasedEquivalence[M, F](new CyclicEquivalence[F](x => true))
  def cyclic(predicate: OpPredicate[F]): Evaluator[M] = this :+ new LiftedFreeBasedEquivalence[M, F](new CyclicEquivalence[F](predicate))
  def transpose(predicate: OpPredicate[F]): Evaluator[M] = this :+ new LiftedFreeBasedEquivalence[M, F](new TransposeEquivalence[F](predicate))
  def :+(e: Equivalence[M]): GenericFreeBasedEvaluator[M, F] = new GenericFreeBasedEvaluator[M, F](equivalences :+ e)
  def real: GenericFreeBasedEvaluator[M, F] = self :+ new AdjointEquivalence[M]
  def symmetric[G](grp: Grp[G])(implicit action: Action[M#Monomial, G]): GenericFreeBasedEvaluator[M, F] = self :+ new SymmetryEquivalence(grp)
}
*/