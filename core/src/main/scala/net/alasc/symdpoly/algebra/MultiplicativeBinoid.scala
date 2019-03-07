package net.alasc.symdpoly.algebra

import cats.kernel.Eq
import spire.algebra.MultiplicativeMonoid

/** Describes a multiplicative monoid that has a absorbing/zero element.
  *
  * Its laws are described in net.alasc.symdpoly.laws.ExtraMultiplicativeMonoidLaws.
  *
  */
trait MultiplicativeBinoid[A] extends MultiplicativeMonoid[A] {
  /** Zero element. */
  def zero: A

  /** Tests whether a binoid element is the zero element. */
  def isZero(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, zero)
}

object MultiplicativeBinoid {

  /** Enables short syntax to retrieve a [[MultiplicativeBinoid]] instance. */
  def apply[A](implicit ev: MultiplicativeBinoid[A]): MultiplicativeBinoid[A] = ev

}
