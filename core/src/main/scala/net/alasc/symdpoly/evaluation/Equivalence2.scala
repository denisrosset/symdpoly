package net.alasc.symdpoly
package evaluation

import net.alasc.finite.Grp
import shapeless.Witness
import spire.algebra.Action
import spire.syntax.action._
import spire.syntax.involution._

/** A transformation that generates equivalent monomials under evaluation by the linear functional. */
trait Equivalence2[M <: generic.MonoidDef with Singleton] {
  def apply(mono: M#Monomial): Set[M#Monomial]
}

final class SymmetryEquivalence2[M <: generic.MonoidDef with Singleton, G](val grp: Grp[G])(implicit val action: Action[M#Monomial, G]) extends Equivalence2[M] {
  def apply(mono: M#Monomial): Set[M#Monomial] = grp.iterator.map(g => mono <|+| g).toSet
}

final class FullAdjointEquivalence2[M <: generic.MonoidDef with Singleton:Witness.Aux] extends Equivalence2[M] {
  def M: M = valueOf[M]
  def apply(mono: M#Monomial): Set[M#Monomial] = Set(mono, M.monoInvolution.adjoint(mono))
}

final class FreeBasedEquivalence2[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](val equivalence: Equivalence[M]) extends Equivalence2[M] {
  def M: M = valueOf[M]
  def apply(mono: Mono[M, F]): Set[Mono[M, F]] = {
    val word = mono.normalForm.mutableCopy
    val iterations = equivalence.inPlace()
  }
}