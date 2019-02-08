package net.alasc.symdpoly
package evaluation

import scala.annotation.tailrec

import net.alasc.finite.{Grp, GrpGroup}
import shapeless.Witness
import spire.algebra.Action
import spire.syntax.action._
import spire.syntax.involution._

import net.alasc.symdpoly.generic.GenericPermutation

/** A transformation that generates equivalent monomials under evaluation by the linear functional. */
trait Equivalence2[M <: generic.MonoidDef with Singleton] {
  def apply(mono: M#Monomial): Set[M#Monomial]
}

final class SymmetryEquivalence2[M <: generic.MonoidDef with Singleton, G](val grp: Grp[G])(implicit val action: Action[M#Monomial, G]) extends Equivalence2[M] {
  def apply(mono: M#Monomial): Set[M#Monomial] = grp.iterator.map(g => mono <|+| g).toSet
}

final class AdjointEquivalence2[M <: generic.MonoidDef with Singleton:Witness.Aux] extends Equivalence2[M] {
  def M: M = valueOf[M]
  def apply(mono: M#Monomial): Set[M#Monomial] = Set(mono, M.monoInvolution.adjoint(mono))
}

final class FreeBasedEquivalence2[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](val equivalence: Equivalence[F]) extends Equivalence2[M] {
  def M: M = valueOf[M]
  def apply(mono: M#Monomial): Set[M#Monomial] = {
    val word = (mono: Mono[M, F]).normalForm.mutableCopy
    equivalence.inPlace(word) match {
      case 1 => Set(mono)
      case n =>
        @tailrec def acc(set: Set[Mono[M, F]], i: Int): Set[Mono[M, F]] = {
          val newSet = set + new Mono[M, F](word.immutableCopy)
          if (i + 1 < n) {
            equivalence.inPlace(word)
            acc(newSet, i + 1)
          } else newSet
        }
        acc(Set(mono), 1)
    }
  }
}
