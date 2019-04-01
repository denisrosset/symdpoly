package net.alasc.symdpoly.quotient

import net.alasc.symdpoly.free
import net.alasc.symdpoly.freebased.Mono

trait PairSubstitutions[F <: free.MonoidDef.Aux[F] with Singleton] {
  def apply(op1: F#Op, op2: F#Op): F#MonoType
}
