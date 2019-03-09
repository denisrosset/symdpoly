package net.alasc.symdpoly.generic

import shapeless.Witness

import net.alasc.symdpoly.{generic, valueOf}

/** Equivalence under the adjoint operation. */
final class AdjointEquivalence[M <: generic.MonoidDef with Singleton:Witness.Aux] extends Equivalence[M] {
  def M: M = valueOf[M]
  def apply(mono: M#Monomial): Set[M#Monomial] = Set(mono, M.monoInvolution.adjoint(mono))
}
