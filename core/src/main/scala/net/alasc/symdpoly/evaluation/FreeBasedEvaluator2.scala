package net.alasc.symdpoly
package evaluation

import scala.annotation.tailrec

import shapeless.Witness
import spire.syntax.cfor.cforRange

import net.alasc.finite.Grp
import net.alasc.symdpoly.free.FreePermutation
import net.alasc.symdpoly.math.GrpDecomposition
import net.alasc.symdpoly.{Mono, Phase, Poly, generic, valueOf}
/*
class FreeBasedEvaluator2[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton: Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton](val equivalences: Vector[Equivalence[F]], val optGrp: Option[Grp[FreePermutation[F]]] = None) extends Evaluator2[M] {

  def M: M = valueOf[M]

  implicit def wF: Witness.Aux[F] = M.Free.witness

  val grpDecomposition = {
    import net.alasc.perms.default._
    optGrp.fold(GrpDecomposition.empty[FreePermutation[F]])(grp => GrpDecomposition(grp))
  }

  def reduce(word: free.MutableWord[F], pad: FreeScratchPad2[F]): free.MutableWord[F] = {
    pad.resetWithCopyOf(word)
    if (applyAndCheckForZero(pad)) word.setToZero()
    else word.setToContentOf(pad.array(0))
  }

  def apply(mono: Mono[M, F]): EvaluatedMono2[this.type, M] = {
    val word = mono.data.mutableCopy()
    val pad = FreeScratchPad2[F] // TODO: reuse
    reduce(word, pad)
    new EvaluatedMono2[this.type, M](new Mono[M, F](word.setImmutable()))
  }


  override def apply(poly: Poly[M, F])(implicit d: DummyImplicit): EvaluatedPoly2[this.type, M] = {
    val pad = FreeScratchPad2[F] // TODO: reuse
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

  def isSelfAdjoint: Boolean = equivalences.exists(_.isInstanceOf[Equivalence.FullAdjointEquivalence[_]])

  def applyEquivalenceAndCheckForZero(fsp: FreeScratchPad2[F], equiv: Equivalence[F]): Boolean = {
    var newN = fsp.n
    cforRange(0 until fsp.n) { i =>
      fsp.scratch(newN).setToContentOf(fsp.scratch(i))
      val order = equiv.inPlace(fsp.scratch(newN))
      if (order > 1) {
        newN += 1
        cforRange(2 until order) { j => // we do not need the index
          fsp.scratch(newN).setToContentOf(fsp.scratch(newN - 1))
          equiv.inPlace(fsp.scratch(newN))
          newN += 1
        }
      }
    }
    cforRange(fsp.n until newN) { i =>
      M.inPlaceNormalForm(fsp.scratch(i))
    }
    fsp.n = newN
    fsp.removeDuplicatesAndCheckForZero()
  }

  def applyEquivalencesAndCheckForZero(fsp: FreeScratchPad2[F]): Boolean = {
    @tailrec def iter(i: Int): Boolean =
      if (i == equivalences.length) false
      else if (applyEquivalenceAndCheckForZero(fsp, equivalences(i))) true
      else iter(i + 1)
    iter(0)
  }

  def applyElementsAndCheckForZero(fsp: FreeScratchPad2[F], elements: Vector[FreePermutation[F]]): Boolean = {
    fsp.ensureMinimalSize(fsp.n * elements.length)
    var newN = fsp.n
    cforRange(1 until elements.length) { i =>
      cforRange(0 until fsp.n) { j =>
        fsp.array(newN).setToContentOf(fsp.array(j))
        fsp.array(newN).applyGenPermAction(elements(i).genPerm)
        M.inPlaceNormalForm(fsp.array(newN))
        newN += 1
      }
    }
    fsp.n = newN
    fsp.removeDuplicatesAndCheckForZero()
  }

  def applyGrpAndCheckForZero(fsp: FreeScratchPad2[F]): Boolean = {
    @tailrec def iter(chain: List[Vector[FreePermutation[F]]]): Boolean = chain match {
      case Nil => false
      case hd :: tl =>
        if (applyElementsAndCheckForZero(fsp, hd)) true
        else iter(tl)
    }
    iter(grpDecomposition.transversals)
  }

  def applyAndCheckForZero(fsp: FreeScratchPad2[F]): Boolean =
    if (applyEquivalencesAndCheckForZero(fsp)) true
    else applyGrpAndCheckForZero(fsp)

}
*/