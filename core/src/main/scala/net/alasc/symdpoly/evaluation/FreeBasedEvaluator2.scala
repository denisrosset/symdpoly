package net.alasc.symdpoly
package evaluation

import scala.annotation.tailrec

import shapeless.Witness
import spire.algebra.Action
import spire.syntax.cfor._
import spire.syntax.std.seq._

import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.Equivalence.{CyclicEquivalence, TransposeEquivalence}
import net.alasc.symdpoly.generic.{FreeBasedPermutation, FreeBasedPermutationMonoAction}
import net.alasc.symdpoly.math.GrpDecomposition

final class FreeBasedEvaluator2[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton: Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](equivalences: Seq[Equivalence2[M]]) extends Evaluator2[M](equivalences) { self =>

  def F: F = M.Free
  implicit def witnessF: Witness.Aux[F] = F.witness

  type E = FreeBasedEvaluator2[M, F]

  type ScratchPad = FreeScratchPad2[F]
  def makeScratchPad: FreeScratchPad2[F] = FreeScratchPad2.apply[F]

  def apply(mono: Mono[M, F], pad: FreeScratchPad2[F]): EvaluatedMono2[this.type, M] = {
    val word = mono.data.mutableCopy()
    reduce(word, pad)
    new EvaluatedMono2[this.type, M](new Mono[M, F](word.setImmutable()))
  }

  override def apply(poly: Poly[M, F], pad: FreeScratchPad2[F])(implicit d: DummyImplicit): EvaluatedPoly2[this.type, M] = {
    val resPoly = free.MutablePoly.empty[F](poly.nTerms)
    cforRange(0 until poly.nTerms) { i =>
      val word = poly.monomialNormalForm(i).mutableCopy()
      reduce(word, pad)
      val coeff = poly.coeff(i) * word.phase.toCyclo
      word.setPhase(Phase.one)
      resPoly.add(word.setImmutable(), coeff)
    }
    new EvaluatedPoly2[this.type, M](resPoly.immutableCopy)
  }

  def applyAndCheckForZero(pad: FreeScratchPad2[F]): Boolean =
    equivalences.exists { // bail out when any equivalence has detected that the monomial is zero
      case fbe: FreeBasedEquivalence2[M, F] => fbe.expandAndCheckForZero(pad)
      case e =>
        val n = pad.n
        val monos = Array.tabulate(n)(i => pad.scratch(i).immutableCopy)
        var ind = 0
        cforRange(0 until n) { i =>
          val mono = new Mono[M, F](monos(i))
          e(mono).foreach { newMono =>
            pad.scratch(ind).setToContentOf(newMono.data)
            ind += 1
          }
        }
        pad.n = ind
        pad.removeDuplicatesAndCheckForZero()
    }

  def reduce(word: free.MutableWord[F], pad: FreeScratchPad2[F]): free.MutableWord[F] = {
    M.inPlaceNormalForm(word)
    if (word.isZero) word else {
      pad.resetWithCopyOf(word)
      if (applyAndCheckForZero(pad)) word.setToZero()
      else word.setToContentOf(pad.array(0))
    }
  }


  def :+(e: Equivalence2[M]): FreeBasedEvaluator2[M, F] = new FreeBasedEvaluator2[M, F](equivalences :+ e)

  def adjoint: FreeBasedEvaluator2[M, F]  = self :+ new AdjointFreeBasedEquivalence2[M, F]

  def symmetric[G](grp: Grp[G])(implicit action: Action[Mono[M, F], G]): FreeBasedEvaluator2[M, F]  = {
    val e = action match {
      case fbpma: FreeBasedPermutationMonoAction[M, F] =>
        val grp1 = grp.asInstanceOf[Grp[FreeBasedPermutation[M, F]]]
        new SymmetryFreeBasedEquivalence2[M, F](grp1)
      case _ =>
        new SymmetryEquivalence2[M, G](grp)
    }
    self :+ e
  }

  def cyclic(predicate: OpPredicate[F]): Evaluator2[M] = self :+ new LiftedFreeBasedEquivalence2[M, F](new CyclicEquivalence[F](predicate))

  def transpose(predicate: OpPredicate[F]): Evaluator2[M] = self :+ new LiftedFreeBasedEquivalence2[M, F](new TransposeEquivalence[F](predicate))

}
