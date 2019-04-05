package net.alasc.symdpoly
package evaluation
package components

import scala.annotation.tailrec

import shapeless.Witness

/** Equivalence under partial transpose operation. */
final case class TransposeComponent[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](val predicate: OpPredicate[F])(implicit val witnessM: Witness.Aux[M]) extends PredicateComponent[M, F] {

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

  def isSelfAdjoint: Boolean = false

}
