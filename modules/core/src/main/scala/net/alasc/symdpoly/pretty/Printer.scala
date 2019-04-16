package net.alasc.symdpoly
package pretty

/** Describes the ability to pretty print instances of A in the format F. */
trait Printer[-A, F <: Format with Singleton] {
  def apply(a: A): F#Output
}

object Printer {
  /** Returns an implicit instance of Pretty. */
  def apply[A, F <: Format with Singleton](implicit ev: Printer[A, F]): Printer[A, F] = ev
}
