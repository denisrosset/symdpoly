package net.alasc.symdpoly
package evaluation

import net.alasc.finite.Grp
import net.alasc.symdpoly.math.GenPerm
import shapeless.Witness
import spire.syntax.cfor.cforRange

import net.alasc.symdpoly.algebra.Phased
import spire.algebra.Order

import net.alasc.symdpoly.Mono

class FreeBasedEvaluator[M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](val equivalences: Seq[Equivalence[F]],
                                                                                                                           val isSelfAdjoint: Boolean)
                                                                                                                          (implicit val wM: Witness.Aux[M]) extends Evaluator[M] {
  self =>

  import FreeBasedEvaluator.FreeScratchPad

  implicit def wF: Witness.Aux[F] = (M.Free: F).witness
  type ScratchPad = FreeScratchPad[F]
  def makeScratchPad: FreeScratchPad[F] = FreeScratchPad[F]

  /** Reduces the given monomial using the given scratch pad.
    * Copies the reduced monomial in the mutable word given as a parameter, so
    * that the scratch pad can be reused. */
  def reduceInScratchPad(mono: free.MutableWord[F], groupElements: Array[GenPerm], pad: ScratchPad = makeScratchPad): Unit = {
    pad.resetWithCopyOf(mono)
    cforRange(0 until equivalences.length) { i =>
      pad.applyEquivalence(equivalences(i))
      cforRange(0 until pad.n) { j =>
        M.inPlaceNormalForm(pad.scratch(j))
      }
    }
    var minimal = pad.scratch(pad.n)
    minimal.setToContentOf(pad.scratch(0))
    var test = pad.scratch(pad.n + 1)
    cforRange(0 until groupElements.length) { si =>
      val g = groupElements(si)
      cforRange(0 until pad.n) { i =>
        if (pad.scratch(i).isZero) { // TODO: prove that this should not happen
          mono.setToZero()
          return
        }
        test.setToContentOf(pad.scratch(i))
        test.applyGenPermAction(g)
        M.inPlaceNormalForm(test)
        if (test.isZero) {
          mono.setToZero()
          return
        }
        minimal.compareToIgnoringPhase(test) match {
          case -1 => // still minimal
          case 0 if minimal.phase != test.phase =>
            mono.setToZero()
            return
          case 0 => // ok, same element and phases match
          case 1 =>
            val tmp = test
            test = minimal
            minimal = tmp
        }
      }
    }
    mono.setToContentOf(minimal)
  }

  def apply(mono: Mono[M, F], group: Grp[GenPerm], pad: FreeScratchPad[F]): EvaluatedMono[self.type, M, group.type] = {
    val word = mono.data.mutableCopy()
    reduceInScratchPad(word, group.iterator.toArray, pad)
    new EvaluatedMono[self.type, M, group.type](new Mono[M, F](word.setImmutable()))
  }

  def apply(poly: Poly[M, F], group: Grp[GenPerm], pad: FreeScratchPad[F])(implicit d: DummyImplicit): EvaluatedPoly[self.type, M, group.type] = {
    val resPoly = free.MutablePoly.empty[F](poly.nTerms)
    cforRange(0 until poly.nTerms) { i =>
      val word = poly.monomialNormalForm(i).mutableCopy()
      reduceInScratchPad(word, group.iterator.toArray, pad)
      val coeff = poly.coeff(i) * word.phase.toCyclo
      word.setPhase(Phase.one)
      resPoly.add(word.setImmutable(), coeff)
    }
    new EvaluatedPoly[self.type, M, group.type](resPoly.immutableCopy)
  }

  private[this] val evaluatedMonoOrderInstance: Order[EvaluatedMono[self.type, M, M#TrivialGroup]]
    = Order.by[EvaluatedMono[self.type, M, M#TrivialGroup], M#Monomial](em => em.normalForm)(M.monoOrder)

  private[this] val evaluatedMonoPhasedInstance: Phased[EvaluatedMono[self.type, M, M#TrivialGroup]]
    = new EvaluatedMonoPhased[self.type, M, F, M#TrivialGroup]

  def evaluatedMonoOrder[G <: Grp[GenPerm] with Singleton]: Order[EvaluatedMono[self.type, M, G]] =
    evaluatedMonoOrderInstance.asInstanceOf[Order[EvaluatedMono[self.type, M, G]]]

  def evaluatedMonoPhased[G <: Grp[GenPerm] with Singleton]: Phased[EvaluatedMono[self.type, M, G]] =
    evaluatedMonoPhasedInstance.asInstanceOf[Phased[EvaluatedMono[self.type, M, G]]]

}


object FreeBasedEvaluator {

  class FreeScratchPad[F <: free.MonoidDef with Singleton](var array: Array[free.MutableWord[F]], var n: Int)(implicit wF: Witness.Aux[F]) { self =>

    override def toString: String = {
      val first = (0 until n).map(i => i.toString.padTo(4, ' ') + "* " + array(i).toString + " " + System.identityHashCode(array(i)).toString)
      val second = (n until array.length).map(i => i.toString.padTo(4, ' ') + "  " + array(i).toString + " " + System.identityHashCode(array(i)).toString)
      (first ++ second).mkString("\n")
    }

    /** Clears the scratch pad and sets its first element to the given word. */
    def resetWithCopyOf(mono: free.MutableWord[F]): FreeScratchPad[F] = {
      scratch(0).setToContentOf(mono)
      n = 1
      self
    }

    def scratch(i: Int): free.MutableWord[F] = {
      if (i >= array.length) ensureMinimalSize(spire.math.max(i + 1, 2 * array.length))
      assert(array(i) ne null)
      array(i)
    }

    def applyEquivalence(equiv: Equivalence[F]): Unit = {
      var newN = n
      cforRange(0 until n) { i =>
        scratch(newN).setToContentOf(scratch(i))
        val order = equiv.inPlace(scratch(newN))
        if (order > 1) {
          newN += 1
          cforRange(2 until order) { j => // we do not need the index
            scratch(newN + 1).setToContentOf(scratch(newN))
            equiv.inPlace(scratch(newN + 1))
            newN += 1
          }
        }
      }
      n = newN
    }
    /*
    def removeDuplicates(): Unit = {
      // implement merge sort with duplicate removal
      // See Bitton, Dina, and David J. DeWitt. “Duplicate Record Elimination in Large Data Files.”
      // ACM Trans. Database Syst. 8, no. 2 (June 1983): 255–265. https://doi.org/10.1145/319983.319987.
    }*/

    def setSize(newSize: Int): Unit = {
      require(newSize >= n)
      if (newSize != n) {
        val newScratch = new Array[free.MutableWord[F]](newSize)
        Array.copy(array, 0, newScratch, 0, array.length)
        val reservedSize = if (n == 0) 8 else newScratch(0).reservedSize
        cforRange(array.length until newScratch.length) { i =>
          newScratch(i) = free.MutableWord.empty[F](reservedSize)
        }
        array = newScratch
      }
    }

    def ensureMinimalSize(minimalSize: Int): Unit = setSize(spire.math.max(minimalSize, array.length))

  }

  object FreeScratchPad {
    def apply[F <: free.MonoidDef with Singleton:Witness.Aux]: FreeScratchPad[F] = new FreeScratchPad[F](Array.empty[free.MutableWord[F]], 0)
  }

}