package net.alasc.symdpoly.internal

import shapeless.Witness
import spire.syntax.cfor._

import metal.IsVPtr
import metal.mutable.{Buffer, HashMap}
import metal.syntax._

import net.alasc.perms.Perm
import net.alasc.symdpoly.{Mono, OrderedSet, free, generic}
import net.alasc.util._

/** Describes an ordered set of equivalence classes of monomials under evaluation. */
class MomentSet[M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux, F <: free.MonoidDef.Aux[F] with Singleton](val monomials: OrderedSet[Mono[M, F]],
                                                                                                                              private[this] val _conjugateIndices: Array[Int]) {
  require(_conjugateIndices.length == monomials.length)
  def nMonomials: Int = monomials.length
  def conjugateIndex(i: Int): Int = _conjugateIndices(i)
  def isSelfAdjoint(i: Int): Boolean = conjugateIndex(i) == i
  def apply(i: Int): Mono[M, F] = monomials(i)
  def indexOf(mono: Mono[M, F]): Int = monomials.indexOf(mono)

  /** Returns whether all monomials in that set are self-adjoint. */
  def allSelfAdjoint: Boolean = {
    cforRange(0 until nMonomials) { i =>
      if (conjugateIndex(i) != i) return false
    }
    true
  }

}

class MomentSetBuilder[F <: free.MonoidDef.Aux[F] with Singleton](val sequence: Buffer[free.MutableWord[F]],
                                                                  val conjugate: Buffer[Int],
                                                                  val momentMap: HashMap[free.MutableWord[F], Int],
                                                                  var n: Int)(implicit wF: Witness.Aux[F]) {
  def F: F = wF.value

  /** Constructs the sorted array of moments and returns it along
    * with the permutation from unsorted to sorted indices.
    */
  def result[M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux](): (MomentSet[M, F], Perm) = {
    val n = sequence.length
    val sortedToUnsorted = Perm.sorting(sequence.toScala) // such that sequence(perm(i)) is sorted for i = 0,1,...
    val unsortedToSorted = sortedToUnsorted.inverse
    val sortedMoments = Array.tabulate(n)( i => (new Mono[M, F](sequence(sortedToUnsorted.image(i)))).asInstanceOf[AnyRef] )
    val conjugateSorted = Array.tabulate(n)( i => unsortedToSorted.image(conjugate(sortedToUnsorted.image(i))) )
    val momentSet = new MomentSet(new OrderedSet[Mono[M, F]](sortedMoments), conjugateSorted)
    (momentSet, unsortedToSorted)
  }

  def getElement(mono: free.MutableWord[F], monoAdjoint: free.MutableWord[F]): Tuple2Int =
    momentMap.ptrFind(mono) match {
      case IsVPtr(vp) =>
        val monoIndex = vp.value
        val monoAdjointIndex = conjugate(monoIndex)
        Tuple2Int(monoIndex, monoAdjointIndex)
      case _ =>
        val immutable = mono.immutableCopy
        val immutableAdjoint = monoAdjoint.immutableCopy
        val monoIndex = n
        val monoAdjointIndex = n + 1
        momentMap(immutable) = monoIndex
        momentMap(immutableAdjoint) = monoAdjointIndex
        sequence += immutable
        sequence += immutableAdjoint
        conjugate += monoAdjointIndex
        conjugate += monoIndex
        n += 2
        Tuple2Int(monoIndex, monoAdjointIndex)
    }

  def getElement(monoSelfAdjoint: free.MutableWord[F]): Int =
    momentMap.ptrFind(monoSelfAdjoint) match {
    case IsVPtr(vp) => vp.value
    case _ =>
      val immutable = monoSelfAdjoint.immutableCopy
      val monoIndex = n
      momentMap(immutable) = monoIndex
      conjugate += monoIndex
      sequence += immutable
      n += 1
      monoIndex
  }

}

object MomentSetBuilder {

  def make[F <: free.MonoidDef.Aux[F] with Singleton](implicit wF: Witness.Aux[F]): MomentSetBuilder[F] = {
    def F: F = wF.value
    val empty = F.one.data
    val sequence = Buffer(empty)
    val conjugate = Buffer(0)
    val momentMap = HashMap.apply(empty -> 0)
    new MomentSetBuilder[F](sequence, conjugate, momentMap, 1)
  }

}
