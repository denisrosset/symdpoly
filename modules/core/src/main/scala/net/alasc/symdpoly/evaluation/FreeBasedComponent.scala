package net.alasc.symdpoly.evaluation

import net.alasc.symdpoly.{free, freebased}
import shapeless.Witness

trait FreeBasedComponent[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends Component[M] { self =>
  def F: F = M.Free
  implicit def witnessF: Witness.Aux[F] = M.witnessFree
}
