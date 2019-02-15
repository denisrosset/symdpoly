package net.alasc.symdpoly.algebra

import cats.kernel.Eq
import spire.algebra.MultiplicativeMonoid

/** Describes a multiplicative monoid that has a absorbing/zero element. */
trait MultiplicativeBinoid[A] extends MultiplicativeMonoid[A] {
  def zero: A
  def isZero(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, zero)
}
