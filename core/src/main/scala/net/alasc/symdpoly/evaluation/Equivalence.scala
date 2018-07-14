package net.alasc.symdpoly
package evaluation

import shapeless.Witness

import scala.annotation.tailrec
import scala.collection.immutable.BitSet

trait OpPredicate[F <: free.MonoidDef with Singleton] {
  def apply(op: F#Op): Boolean
}

/** A transformation that generates equivalent monomials under evaluation by the linear functional. */
trait Equivalence[F <: free.MonoidDef with Singleton] {

  /** Applies the transformation and returns an integer such that the transformation iterated n times
    * is the identity. The returned n may not necessarily be the smallest integer having that property.
    *
    * If n == 1, then the transformation is the identity and the given monomial has not been changed.
    */
  def inPlace(mono: free.MutableWord[F]): Int

  /** Returns an upper bound on the number of elements returned by this equivalence transformation for
    * monomial of bounded degree.
    */
  def upperBound(degree: Int): Int
}

object Equivalence {
  def fullAdjoint[F <: free.MonoidDef with Singleton:Witness.Aux]: Equivalence[F] = new FullAdjointEquivalence[F]
  def cyclic[F <: free.MonoidDef with Singleton:Witness.Aux](predicate: OpPredicate[F]): Equivalence[F] = new CyclicEquivalence[F](predicate)
  def transpose[F <: free.MonoidDef with Singleton:Witness.Aux](predicate: OpPredicate[F]): Equivalence[F] = new TransposeEquivalence[F](predicate)

  class CyclicEquivalence[F <: free.MonoidDef with Singleton:Witness.Aux](predicate: OpPredicate[F]) extends PredicateEquivalence[F](predicate) {
    /** Performs the in place cyclic permutation of the group of operators selected by the given predicate.
      * Elements are shifted one place to the left.
      */
    def inPlace(mono: free.MutableWord[F]): Int = {
      val n = mono.length

      @tailrec def findNext(i: Int): Int =
        if (i == n) n
        else if (predicateIndex(mono.indices(i))) i
        else findNext(i + 1)

      val first = findNext(0)
      if (first == n) return 1
      val second = findNext(first)
      if (second == n) return 1

      @tailrec def iterate(prev: Int, current: Int, count: Int): Int =
        if (current == n) {
          count
        } else {
          mono.swap(prev, current)
          iterate(current, findNext(current + 1), count + 1)
        }

      iterate(first, second, 0)
    }

    def upperBound(degree: Int): Int = degree
  }

  class TransposeEquivalence[F <: free.MonoidDef with Singleton:Witness.Aux](predicate: OpPredicate[F]) extends PredicateEquivalence[F](predicate) {
    /** Performs the in place partial transpose of the group of operators selected by the given predicate.
      *
      * Returns whether any changes have been made.
      */
    def inPlace(mono: free.MutableWord[F]): Int =
      if (mono.isZero || mono.isOne || mono.isMinusOne) 1 else {
        @tailrec def iter(l: Int, r: Int, changed: Boolean): Boolean =
          if (l > r) changed
          else if (l == r) {
            if (predicateIndex(mono.indices(l))) {
              val la = F.indexAdjoint(mono.indices(l))
              if (la != mono.indices(l)) {
                mono.indices(l)
                true
              } else changed
            } else changed
          } else if (predicateIndex(mono.indices(l)) && predicateIndex(mono.indices(r))) {
            val la = F.indexAdjoint(mono.indices(l))
            if (la != mono.indices(r)) {
              val ra = F.indexAdjoint(mono.indices(r))
              mono.indices(l) = ra
              mono.indices(r) = la
              iter(l + 1, r - 1, true)
            }
            else iter(l + 1, r - 1, changed)
          } else if (predicateIndex(mono.indices(l))) iter(l, r - 1, changed)
          else if (predicateIndex(mono.indices(r))) iter(l + 1, r, changed)
          else iter(l + 1, r - 1, changed)

        if (iter(0, mono.length - 1, false)) 2 else 1
      }

    def upperBound(degree: Int): Int = 2
  }

}

abstract class PredicateEquivalence[F <: free.MonoidDef with Singleton](predicate: OpPredicate[F])(implicit wF: Witness.Aux[F]) extends Equivalence[F] {
  def F: F = wF.value
  val predicateIndex: BitSet = BitSet.empty ++ (0 until F.nOperators).filter(i => predicate(F.opFromIndex(i)))
}
