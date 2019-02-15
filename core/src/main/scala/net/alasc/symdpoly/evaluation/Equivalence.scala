package net.alasc.symdpoly
package evaluation

import scala.annotation.tailrec

import net.alasc.finite.{Grp, GrpGroup}
import shapeless.Witness
import spire.algebra.Action
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.involution._

import net.alasc.symdpoly.generic.{FreeBasedMono, FreeBasedPermutation}
import net.alasc.symdpoly.math.GrpDecomposition

/** A transformation that generates equivalent monomials under evaluation by the linear functional. */
trait Equivalence[M <: generic.MonoidDef with Singleton] {
  def apply(mono: M#Monomial): Set[M#Monomial]
}

final class SymmetryEquivalence[M <: generic.MonoidDef with Singleton, G](val grp: Grp[G])(implicit val action: Action[M#Monomial, G]) extends Equivalence[M] {
  def apply(mono: M#Monomial): Set[M#Monomial] = grp.iterator.map(g => mono <|+| g).toSet
}

final class AdjointEquivalence[M <: generic.MonoidDef with Singleton:Witness.Aux] extends Equivalence[M] {
  def M: M = valueOf[M]
  def apply(mono: M#Monomial): Set[M#Monomial] = Set(mono, M.monoInvolution.adjoint(mono))
}

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
            pad.scratch(ind).applyGenPermAction(hd(i).genPerm)
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
