package net.alasc.symdpoly.freebased

import cats.evidence.Is
import spire.algebra.MultiplicativeMonoid

import net.alasc.symdpoly.free

/** Mixin trait offering methods that treat other objects (such as operator variables) as monomials. */
trait MonoLike[M <: MonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton] extends PolyLike[M, F] {
  lhs =>

  // Abstract methods to overload
  def toMono: Mono[M, F]

  def *(rhs: Mono[M, F])(implicit mm: MultiplicativeMonoid[Mono[M, F]]): Mono[M, F]

  def pow(rhs: Int)(implicit mm: MultiplicativeMonoid[Mono[M, F]]): Mono[M, F] = toMono.pow(rhs)

  def *(rhs: F#Op)(implicit mm: MultiplicativeMonoid[Mono[M, F]], ev: Mono[F, F] Is Mono[M, F]): Mono[M, F] = lhs * ev.coerce(rhs.toMono)

  def *(rhs: F#PhasedOp)(implicit mm: MultiplicativeMonoid[Mono[M, F]], ev: Mono[F, F] Is Mono[M, F]): Mono[M, F] = lhs * (ev.coerce(rhs.toMono): Mono[M, F])

}
