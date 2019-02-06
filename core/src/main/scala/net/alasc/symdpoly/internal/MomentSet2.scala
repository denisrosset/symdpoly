package net.alasc.symdpoly
package internal

import shapeless.Witness
import spire.syntax.cfor._

import metal.IsVPtr
import metal.mutable.{Buffer, HashMap}
import metal.syntax._

import net.alasc.perms.Perm
import net.alasc.symdpoly.evaluation.{EvaluatedMono2, Evaluator2}
import net.alasc.symdpoly.{Mono, OrderedSet, free, generic}
import net.alasc.util._
import metal.syntax._

// TODO: optimize the things below for evaluated monomials with a normal form as a free monomial

/** Describes an ordered set of equivalence classes of monomials under evaluation. */
class MomentSet2[
  E <: Evaluator2[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux
](val elements: OrderedSet[EvaluatedMono2[E, M]], private[this] val _conjugateIndices: Array[Int]) {
  require(_conjugateIndices.length == elements.length)
  def nElements: Int = elements.length
  def conjugateIndex(i: Int): Int = _conjugateIndices(i)
  def isSelfAdjoint(i: Int): Boolean = conjugateIndex(i) == i
  def apply(i: Int): EvaluatedMono2[E, M] = elements(i)
  def indexOf(element: EvaluatedMono2[E, M]): Int = elements.indexOf(element)

  /** Returns whether all monomials in that set are self-adjoint. */
  def allSelfAdjoint: Boolean = {
    cforRange(0 until nElements) { i =>
      if (conjugateIndex(i) != i) return false
    }
    true
  }

}

class MomentSetBuilder2[
  E <: Evaluator2[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux
](val sequence: Buffer[EvaluatedMono2[E, M]],
  val conjugate: Buffer[Int],
  val momentMap: HashMap[EvaluatedMono2[E, M], Int],
  var n: Int) {
  def E: E = valueOf[E]
  def M: M = valueOf[M]

  /** Constructs the sorted array of moments and returns it along
    * with the permutation from unsorted to sorted indices.
    */
  def result(): (MomentSet2[E, M], Perm) = {
    val n = sequence.length
    val sortedToUnsorted = Perm.sorting(sequence.toScala) // such that sequence(perm(i)) is sorted for i = 0,1,...
    val unsortedToSorted = sortedToUnsorted.inverse
    val sortedMoments = Array.tabulate(n)( i => sequence(sortedToUnsorted.image(i)).asInstanceOf[AnyRef] )
    val conjugateSorted = Array.tabulate(n)( i => unsortedToSorted.image(conjugate(sortedToUnsorted.image(i))) )
    val momentSet = new MomentSet2(new OrderedSet[EvaluatedMono2[E, M]](sortedMoments), conjugateSorted)
    (momentSet, unsortedToSorted)
  }

  def getElement(mono: EvaluatedMono2[E, M], monoAdjoint: EvaluatedMono2[E, M]): Tuple2Int =
    momentMap.ptrFind(mono) match {
      case IsVPtr(vp) =>
        val monoIndex = vp.value
        val monoAdjointIndex = conjugate(monoIndex)
        Tuple2Int(monoIndex, monoAdjointIndex)
      case _ =>
        val monoIndex = n
        val monoAdjointIndex = n + 1
        momentMap(mono) = monoIndex
        momentMap(monoAdjoint) = monoAdjointIndex
        sequence += mono
        sequence += monoAdjoint
        conjugate += monoAdjointIndex
        conjugate += monoIndex
        n += 2
        Tuple2Int(monoIndex, monoAdjointIndex)
    }

  def getElement(monoSelfAdjoint: EvaluatedMono2[E, M]): Int =
    momentMap.ptrFind(monoSelfAdjoint) match {
      case IsVPtr(vp) => vp.value
      case _ =>
        val monoIndex = n
        momentMap(monoSelfAdjoint) = monoIndex
        conjugate += monoIndex
        sequence += monoSelfAdjoint
        n += 1
        monoIndex
    }

}

object MomentSetBuilder2 {

  def make[E <: Evaluator2[M] with Singleton:Witness.Aux, M <: generic.MonoidDef with Singleton:Witness.Aux]: MomentSetBuilder2[E, M] = {
    val empty = valueOf[E].apply(valueOf[M].one)
    val sequence = Buffer(empty)
    val conjugate = Buffer(0)
    val momentMap = HashMap.apply(empty -> 0)
    new MomentSetBuilder2[E, M](sequence, conjugate, momentMap, 1)
  }

}
