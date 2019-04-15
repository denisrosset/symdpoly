package net.alasc.symdpoly.quotient

import shapeless.Witness

import net.alasc.symdpoly.free
import net.alasc.symdpoly.freebased.Mono

trait PairSubstitutions[F <: free.MonoDef.Aux[F] with Singleton] {
  def apply(op1: F#Op, op2: F#Op): F#MonoType
}

object PairSubstitutions {

  implicit class SafePairSubstitutions[F <: free.MonoDef.Aux[F] with Singleton: Witness.Aux](val ps: PairSubstitutions[F]) {
    def safeApply(op1: F#Op, op2: F#Op): F#MonoType = try {
      ps.apply(op1, op2)
    } catch {
      case me: MatchError => op1 * op2
    }
  }

}
