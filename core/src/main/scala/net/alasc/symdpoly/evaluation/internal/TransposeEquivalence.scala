package net.alasc.symdpoly
package freebased

import scala.annotation.tailrec

import shapeless.Witness

/*
final class TransposeEquivalence[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](val predicate: OpPredicate[F])(implicit val witnessM: Witness.Aux[M]) extends PredicateEquivalence[M, F] {

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

}

final class TransposeEquivalences[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](val predicates: OpPredicate[F])(implicit val witnessM: Witness.Aux[M]) extends PredicateEquivalence[M, F] {
}
*/