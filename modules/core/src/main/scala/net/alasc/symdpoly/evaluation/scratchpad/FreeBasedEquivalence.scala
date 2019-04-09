package net.alasc.symdpoly
package evaluation
package scratchpad

import shapeless.Witness

import net.alasc.symdpoly.{free, freebased}

trait FreeBasedEquivalence[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends Equivalence[M] with FreeBasedComponent[M, F] {

}
