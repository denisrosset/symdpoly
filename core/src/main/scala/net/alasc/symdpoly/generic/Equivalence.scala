package net.alasc.symdpoly
package generic

/** An equivalence relation on monomials. */
abstract class Equivalence[M <: generic.MonoidDef with Singleton] {

  /** Returns the set of monomials equivalent to the given monomial. */
  def apply(mono: M#Monomial): Set[M#Monomial]
}
