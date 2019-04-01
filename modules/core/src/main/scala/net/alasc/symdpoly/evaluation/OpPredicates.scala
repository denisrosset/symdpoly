package net.alasc.symdpoly
package evaluation

/** Boolean function on operator variables. */
trait OpPredicate[F <: free.MonoidDef with Singleton] {
  def apply(op: F#Op): Boolean
}

/** Boolean function on pairs of operator variables. */
trait OpPredicate2[F <: free.MonoidDef with Singleton] {
  def apply(op1: F#Op, op2: F#Op): Boolean
}
