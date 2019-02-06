package net.alasc.symdpoly
package evaluation


import scala.annotation.tailrec

import cats.{Contravariant, Invariant}

import net.alasc.symdpoly.{Maximization, Mono, OrderedSet, Phase, Poly, free, generic, valueOf}
import net.alasc.symdpoly.math.GenPerm
import shapeless.Witness
import spire.algebra._
import spire.syntax.cfor.cforRange
import spire.syntax.order._
import spire.syntax.group._
import algebra.Instances._
import cyclo.Cyclo
import scalin.immutable.{Vec, VecEngine}
import net.alasc.syntax.group._
import net.alasc.finite.Grp
import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.generic.FreeBasedMonoidDef
import cats.instances.order.catsContravariantMonoidalForOrder
import cats.instances.eq.catsContravariantMonoidalForEq

import scalin.syntax.all._
import net.alasc.syntax.group._
import net.alasc.bsgs.{Chain, GrpChain, GrpChainPermutationAction, Node, Term}
import net.alasc.symdpoly.evaluation.FreeBasedEvaluator.FreeScratchPad
import net.alasc.symdpoly.free.{MutableWord, OpGenPerm}

final class EvaluatedMono2[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton:Witness.Aux](val normalForm: M#Monomial) { lhs =>
  def E: E = valueOf[E]

  override def toString: String = "L(" + normalForm.toString + ")"
  override def hashCode: Int = normalForm.hashCode()
  override def equals(any: Any): Boolean = any match {
    case rhs: EvaluatedMono2[E, M] if lhs.E eq rhs.E => E.evaluatedMonoOrder.eqv(lhs, rhs)
    case _ => false
  }

}

object EvaluatedMono2 {
  implicit def order[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton]: Order[EvaluatedMono2[E, M]] = valueOf[E].evaluatedMonoOrder
  implicit def phased[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton]: Phased[EvaluatedMono2[E, M]] = valueOf[E].evaluatedMonoPhased
}

final class EvaluatedPoly2[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton:Witness.Aux](val normalForm: M#Polynomial) { lhs =>

  def E: E = valueOf[E]

  override def toString: String = normalForm.string("L(", ")")
  override def hashCode: Int = normalForm.hashCode()
  override def equals(any: Any): Boolean = any match {
    case rhs: EvaluatedPoly2[E, M] if (lhs.E eq rhs.E) => valueOf[M].polyEq.eqv(lhs.normalForm, rhs.normalForm)
    case _ => false
  }

  def nTerms: Int = normalForm.nTerms
  def monomial(i: Int): EvaluatedMono2[E, M] = new EvaluatedMono2[E, M](normalForm.monomial(i))
  def coeff(i: Int): Cyclo = normalForm.coeff(i)

  def vecOverOrderedSet(orderedSet: OrderedSet[EvaluatedMono2[E, M]])(implicit V: VecEngine[Cyclo]): Vec[Cyclo] =
    V.fromMutable(orderedSet.length, Cyclo.zero) { vec =>
      implicit def monoOrder: Order[EvaluatedMono2[E, M]] = valueOf[E].evaluatedMonoOrder
      cforRange(0 until nTerms) { i =>
        val m = monomial(i)
        val c = coeff(i)
        val j = orderedSet.indexOf(m)
        if (j == -1) sys.error(s"Monomial $m not present in the moment set ${orderedSet}.") else { vec(j) := vec(j) + c }
      }
    }

}

object EvaluatedPoly2 {
  implicit def equ[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton]: Eq[EvaluatedPoly2[E, M]] = valueOf[E].evaluatedPolyEq
  implicit def vectorSpace[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton]: VectorSpace[EvaluatedPoly2[E, M], Cyclo] = valueOf[E].evaluatedPolyVectorSpace
}

trait Evaluator2[M <: generic.MonoidDef with Singleton] { self =>

  val witness: Witness.Aux[self.type] = Witness.mkWitness(self)

  def M: M

  // optimization: set to true if apply(a) == apply(a.adjoint)
  def isSelfAdjoint: Boolean

  def apply(mono: M#Monomial): EvaluatedMono2[self.type, M]

  def apply(poly: M#Polynomial)(implicit d: DummyImplicit): EvaluatedPoly2[self.type, M]

  // typeclasses

  val evaluatedMonoOrder: Order[EvaluatedMono2[self.type, M]] = Contravariant[Order].contramap(M.monoOrder)(em => em.normalForm)
  val evaluatedMonoPhased: Phased[EvaluatedMono2[self.type, M]] = Invariant[Phased].imap(M.monoPhased)(apply)(_.normalForm)
  val evaluatedPolyEq: Eq[EvaluatedPoly2[self.type, M]] = Contravariant[Eq].contramap(M.polyEq)(ep => ep.normalForm)
  val evaluatedPolyVectorSpace: VectorSpace[EvaluatedPoly2[self.type, M], Cyclo] = Invariant[Lambda[V => VectorSpace[V, Cyclo]]].imap(M.polyAssociativeAlgebra)(apply)(_.normalForm)

}

class FreeBasedEvaluator2[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton: Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton](val equivalences: Vector[Equivalence[F]], val optGrp: Option[Grp[OpGenPerm[F]]] = None) extends Evaluator2[M] {

  def M: M = valueOf[M]

  implicit def wF: Witness.Aux[F] = M.Free.witness

  val grpDecomposition = {
    import net.alasc.perms.default._
    optGrp.fold(GrpDecomposition.empty[OpGenPerm[F]])(grp => GrpDecomposition(grp))
  }


  def reduce(word: free.MutableWord[F], pad: FreeScratchPad2[F]): free.MutableWord[F] = {
    pad.resetWithCopyOf(word)
    if (applyGrpAndCheckForZero(pad)) word.setToZero()
    else word.setToContentOf(pad.array(0))
  }

  def apply(mono: Mono[M, F]): EvaluatedMono2[this.type, M] = {
    val word = mono.data.mutableCopy()
    val pad = FreeScratchPad2[F] // TODO: reuse
    reduce(word, pad)
    new EvaluatedMono2[this.type, M](new Mono[M, F](word.setImmutable()))
  }


  def apply(poly: Poly[M, F])(implicit d: DummyImplicit): EvaluatedPoly2[this.type, M] = {
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

  def applyElementsAndCheckForZero(fsp: FreeScratchPad2[F], elements: Vector[OpGenPerm[F]]): Boolean = {
    fsp.ensureMinimalSize(fsp.n * elements.length)
    var newN = fsp.n
    cforRange(1 until elements.length) { i =>
      cforRange(0 until fsp.n) { j =>
        fsp.array(newN).setToContentOf(fsp.array(j))
        fsp.array(newN).applyGenPermAction(elements(i).opAction)
        M.inPlaceNormalForm(fsp.array(newN))
      }
    }
    fsp.n = newN
    fsp.removeDuplicatesAndCheckForZero()
  }

  def applyGrpAndCheckForZero(fsp: FreeScratchPad2[F]): Boolean = {
    @tailrec def iter(chain: List[Vector[OpGenPerm[F]]]): Boolean = chain match {
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

class FreeScratchPad2[F <: free.MonoidDef with Singleton: Witness.Aux](var array: Array[MutableWord[F]], var n: Int) { self =>
  
  def F: F = valueOf[F]

  override def toString: String = {
    val first = (0 until n).map(i => i.toString.padTo(4, ' ') + "* " + array(i).toString + " " + System.identityHashCode(array(i)).toString)
    val second = (n until array.length).map(i => i.toString.padTo(4, ' ') + "  " + array(i).toString + " " + System.identityHashCode(array(i)).toString)
    (first ++ second).mkString("\n")
  }
  
  // Scratchpad manipulation (resize, clear)

  /** Clears the scratch pad and sets its first element to the given word. */
  def resetWithCopyOf(mono: MutableWord[F]): FreeScratchPad2[F] = {
    scratch(0).setToContentOf(mono)
    n = 1
    self
  }

  def scratch(i: Int): MutableWord[F] = {
    if (i >= array.length) ensureMinimalSize(spire.math.max(i + 1, 2 * array.length))
    assert(array(i) ne null)
    array(i)
  }

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

  // Sorting and duplicate removal

  /** Sorts the given range from index `start` up to, but not including, the index `end`,
    * while removing duplicates.
    * Operates in place, and returns the range of the result as Some((start1, end1)) with the
    * same convention for the range.
    * Returns None if the monomial is zero, i.e. we found x and x * phase with phase != 1 in the array.
    */
  def insertionSort(start: Int, end: Int): Option[(Int, Int)] = {
    require(start <= end && start >= 0 && end <= array.length)
    cforRange(start + 1 until end) { i =>
      var item = array(i)
      // Finds the position of array(i)
      @tailrec def findPlace(h: Int): Int =
        if (h <= start) h
        else {
          val c = item.compareToIgnoringPhase(array(h - 1))
          if (c > 0) h // we found the place, bail out
          else if (c == 0 && item.phase != array(h - 1).phase) -1 // element is zero
          else { // c <= 0
            if (c == 0) item = MutableWord.zero[F] // this is a duplicate
            array(h) = array(h - 1)
            findPlace(h - 1)
          }
        }
      val hole = findPlace(i)
      if (hole == -1) return None
      array(hole) = item
    }
    @tailrec def findNewStart(i: Int): Int =
      if (i == end) sys.error("Should never happen")
      else if (array(i).isZero) findNewStart(i + 1)
      else i
    Some((findNewStart(start), end))
  }

  /** Removes duplicates in the scratch array, and returns whether the resulting monomial is zero. */
  def removeDuplicatesAndCheckForZero(): Boolean = {
    // TODO: implement merge sort with duplicate removal
    // See Bitton, Dina, and David J. DeWitt. “Duplicate Record Elimination in Large Data Files.”
    // ACM Trans. Database Syst. 8, no. 2 (June 1983): 255–265. https://doi.org/10.1145/319983.319987.
    insertionSort(0, n) match {
      case None => true // the monomial is zero
      case Some((newStart, newEnd)) =>
        val newN = newEnd - newStart
        System.arraycopy(array, newStart, array, 0, newN)
        n = newN
        false // the monomial is not (yet) proven to be zero
    }
  }

}

/** Decomposition of a group in a sequence of transversals, such that every group element can be written
  * as d(1)(i1) |+| d(2)(i2) |+| d(3)(i3) ... for indices i1, i2, i3 ... where d = decomposition
  *
  * Additional property: the first element of each transversal is the identity.
  */
class GrpDecomposition[G](val transversals: List[Vector[G]])

object GrpDecomposition {

  def apply[G](grp: Grp[G])(implicit G: GrpChainPermutationAction[G]): GrpDecomposition[G] = {
    import G.{group, equ}
    val chain = G.fromGrp(grp).chain
    @tailrec def buildTransversal(c: Chain[G, _], acc: List[Vector[G]]): List[Vector[G]] = c match {
      case node: Node[G, _] =>
        val transversal = Group[G].id +: node.orbit.filterNot(_ == node.beta).toVector.map(b => node.u(b))
        buildTransversal(node.next, transversal :: acc)
      case _: Term[G, _] => acc
    }
    new GrpDecomposition[G](buildTransversal(chain, Nil))
  }

  def empty[G]: GrpDecomposition[G] = new GrpDecomposition[G](Nil)

}

object FreeScratchPad2 {
  def apply[F <: free.MonoidDef with Singleton:Witness.Aux]: FreeScratchPad2[F] = new FreeScratchPad2[F](Array.empty[free.MutableWord[F]], 0)
}


object Evaluator2 {


}