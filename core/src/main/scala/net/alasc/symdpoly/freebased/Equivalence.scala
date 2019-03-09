package net.alasc.symdpoly
package freebased

import shapeless.Witness

/** An equivalence relation on monomials from a quotient monoid over a free monoid. */
abstract class Equivalence[
M <: MonoidDef.Aux[F] with Singleton:Witness.Aux,
F <: free.MonoidDef.Aux[F] with Singleton
] extends generic.Equivalence[M] {

  def M: M = valueOf[M]
  def F: F = (M.Free: F)

}
/*
import net.alasc.symdpoly.generic.Equivalence
import net.alasc.symdpoly.freebased.FreeBasedMono
import net.alasc.symdpoly.{free, generic, valueOf}
import shapeless.Witness

abstract class FreeBasedEquivalence[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends Equivalence[M] {
  def M: M = valueOf[M]
  def F: F = M.Free
  implicit def witnessF: Witness.Aux[F] = F.witness

  def apply(mono: FreeBasedMono[M, F]): Set[FreeBasedMono[M, F]] = {
    val pad = FreeScratchPad[F]
    pad.scratch(0).setToContentOf(mono.data)
    pad.n = 1
    if (expandAndCheckForZero(pad))
      Set(FreeBasedMono.zero[M, F])
    else
      Set(Seq.tabulate(pad.n)(i => new FreeBasedMono[M, F](pad.scratch(i).setImmutable())): _*)
  }

  /** Expands the monomials in the given scratch pad using this equivalence, and returns true when the resulting monomial is zero. */
  def expandAndCheckForZero(pad: FreeScratchPad[F]): Boolean

}
*/


/*
final class LiftedFreeBasedEquivalence[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](val equivalence: TodoEquivalence[F]) extends FreeBasedEquivalence[M, F] {

  def expandAndCheckForZero(pad: FreeScratchPad[F]): Boolean = {
    var ind = pad.n
    cforRange(0 until pad.n) { i =>
      pad.scratch(ind).setToContentOf(pad.scratch(i))
      val order = equivalence.inPlace(pad.scratch(ind))
      if (order > 1) {
        ind += 1
        cforRange(2 until order) { j => // we do not need the index
          pad.scratch(ind).setToContentOf(pad.scratch(ind - 1))
          equivalence.inPlace(pad.scratch(ind))
          ind += 1
        }
      }
    }
    cforRange(pad.n until ind) { i =>
      (M: M).inPlaceNormalForm(pad.scratch(ind))
    }
    pad.n = ind
    pad.removeDuplicatesAndCheckForZero()
  }

}

final class AdjointFreeBasedEquivalence[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends FreeBasedEquivalence[M, F] {
  def expandAndCheckForZero(pad: FreeScratchPad[F]): Boolean = {
    var ind = pad.n
    cforRange(0 until pad.n) { i =>
      pad.scratch(ind).setToContentOf(pad.scratch(i))
      pad.scratch(ind).inPlaceAdjoint()
      (M: M).inPlaceNormalForm(pad.scratch(ind))
      ind += 1
    }
    pad.removeDuplicatesAndCheckForZero()
  }
}

final class SymmetryFreeBasedEquivalence[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](val grp: Grp[FreeBasedPermutation[M, F]]) extends FreeBasedEquivalence[M, F] {

  import net.alasc.perms.default._

  val decomposition = GrpDecomposition(grp)

  def expandAndCheckForZero(pad: FreeScratchPad[F]): Boolean = {
    @tailrec def rec(t: List[Vector[FreeBasedPermutation[M, F]]]): Boolean = t match {
      case hd :: tl =>
        var ind = pad.n
        cforRange(1 until hd.length) { i =>
          cforRange(0 until pad.n) { j =>
            pad.scratch(ind).setToContentOf(pad.scratch(j))
            pad.scratch(ind).inPlaceGenPermAction(hd(i).genPerm)
            (M: M).inPlaceNormalForm(pad.scratch(ind))
            ind += 1
          }
        }
        pad.n = ind
        if (pad.removeDuplicatesAndCheckForZero())
          {
            true
          } else rec(tl)
      case Nil => false
    }
    rec(decomposition.transversals)
  }

}
*/
