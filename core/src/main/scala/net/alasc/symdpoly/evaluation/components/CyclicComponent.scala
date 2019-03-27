package net.alasc.symdpoly
package evaluation
package components

import scala.annotation.tailrec

import shapeless.Witness

final case class CyclicComponent[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](val predicate: OpPredicate[F])(implicit val witnessM: Witness.Aux[M]) extends PredicateComponent[M, F] {

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

  def isSelfAdjoint: Boolean = false

}
