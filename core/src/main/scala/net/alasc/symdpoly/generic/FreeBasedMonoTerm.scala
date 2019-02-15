package net.alasc.symdpoly.generic

import cats.evidence.Is
import spire.algebra.MultiplicativeMonoid

import net.alasc.symdpoly.free

trait FreeBasedMonoTerm[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton] {
  lhs =>

  // Abstract methods to overload
  def toMono: FreeBasedMono[M, F]
  def *(rhs: FreeBasedMono[M, F])(implicit mm: MultiplicativeMonoid[FreeBasedMono[M, F]]): FreeBasedMono[M, F]

  def pow(rhs: Int)(implicit mm: MultiplicativeMonoid[FreeBasedMono[M, F]]): FreeBasedMono[M, F] = toMono.pow(rhs)

  def *(rhs: F#Op)(implicit mm: MultiplicativeMonoid[FreeBasedMono[M, F]], ev: FreeBasedMono[F, F] Is FreeBasedMono[M, F]): FreeBasedMono[M, F] = lhs * ev.coerce(rhs.toMono)

  def *(rhs: F#PhasedOp)(implicit mm: MultiplicativeMonoid[FreeBasedMono[M, F]], ev: FreeBasedMono[F, F] Is FreeBasedMono[M, F]): FreeBasedMono[M, F] = lhs * (ev.coerce(rhs.toMono): FreeBasedMono[M, F])
}
