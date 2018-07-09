package net.alasc.symdpoly.quotient

import net.alasc.symdpoly.{Mono, free}

trait PairSubstitutions[F <: free.MonoidDef.Aux[F] with Singleton] {
  def apply(op1: F#Op, op2: F#Op): Mono[F, F]
}
