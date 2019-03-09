package net.alasc.symdpoly
package freebased

import scala.annotation.tailrec

import shapeless.Witness

final class AdjointEquivalence[
  M <: MonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends InPlaceEquivalence[M, F] {

  /** Performs the in place transpose of the given monomial.
    *
    * Returns whether any changes have been made.
    */
  def inPlace(mono: free.MutableWord[F]): Int =
    if (mono.isZero || mono.isOne || mono.isMinusOne) 1 else {
      @tailrec def iter(l: Int, r: Int, changed: Boolean): Boolean =
        if (l > r) changed
        else {
          val la = F.indexAdjoint(mono.indices(l))
          if (la != mono.indices(r)) {
            val ra = F.indexAdjoint(mono.indices(r))
            mono.indices(l) = ra
            mono.indices(r) = la
            iter(l + 1, r - 1, true)
          }
          else iter(l + 1, r - 1, changed)
        }

      if (iter(0, mono.length - 1, false)) 2 else 1
    }

}
