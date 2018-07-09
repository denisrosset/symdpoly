package net.alasc.symdpoly.evaluation

import net.alasc.symdpoly.free
import shapeless.Witness

import scala.annotation.tailrec

class FullAdjointEquivalence[F <: free.MonoidDef with Singleton](implicit wF: Witness.Aux[F]) extends Equivalence[F] {
  def F: F = wF.value
  /** Performs the in place partial transpose of the group of operators selected by the given predicate.
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

  def upperBound(degree: Int): Int = 2
}
