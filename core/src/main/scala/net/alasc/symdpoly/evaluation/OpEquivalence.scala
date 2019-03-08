package net.alasc.symdpoly
package evaluation

/** Boolean function on operator variables. */
trait OpPredicate[F <: free.MonoidDef with Singleton] {
  def apply(op: F#Op): Boolean
}
