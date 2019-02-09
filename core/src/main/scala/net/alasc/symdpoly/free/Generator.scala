package net.alasc.symdpoly
package free

import shapeless.Witness

import net.alasc.symdpoly.math.{GenPerm, PhasedInt}

class Generator[F <: free.MonoidDef with Singleton:Witness.Aux] protected[symdpoly](val name: String, val opAction: GenPerm) {
  override def toString: String = name + ": " + generic.FreeBasedPermutation.prettyPrintGenPerm(opAction, valueOf[F])
}
