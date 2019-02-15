package net.alasc.symdpoly.generic

import cats.kernel.Eq
import spire.algebra.{Involution, MultiplicativeMonoid}

import net.alasc.symdpoly.{Mono, Phase, Poly}
import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}

trait GenMono[M <: MonoidDef with Singleton, T <: GenMono[M, T]] { lhs: T =>
  def isZero(implicit mb: MultiplicativeBinoid[T], equ: Eq[T]): Boolean = mb.isZero(lhs)
  def isOne(implicit mm: MultiplicativeMonoid[T], equ: Eq[T]): Boolean = mm.isOne(lhs)
  def unary_- (implicit phased: Phased[T]): T = phased.gtimesl(Phase.minusOne, lhs)
  def *(rhs: Phase)(implicit phased: Phased[T]): T = phased.gtimesr(lhs, rhs)
  def *(rhs: T)(implicit mm: MultiplicativeMonoid[T]): T = mm.times(lhs, rhs)
  def adjoint(implicit inv: Involution[T]): T = inv.adjoint(lhs)
  def pow(rhs: Int)(implicit mm: MultiplicativeMonoid[T]): T = mm.pow(lhs, rhs)
}
