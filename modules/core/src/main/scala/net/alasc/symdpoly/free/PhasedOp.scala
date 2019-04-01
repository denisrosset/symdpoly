package net.alasc.symdpoly
package free

import shapeless.Witness

import net.alasc.symdpoly.freebased.{Mono, Poly}
import net.alasc.symdpoly.generic.{MonoLike, PolyLike}
import net.alasc.symdpoly.math.Phase

/** A trait for objects that can be converted to a [[PhasedOp]] instance.
  *
  * See [[generic.PolyLike]] for the structure behind this construction.
  *
  */
trait PhasedOpLike[M <: MonoidDef.Aux[M] with Singleton] {
  def toPhasedOp: PhasedOp[M]
  def *(rhs: Phase)(implicit d: DummyImplicit): PhasedOp[M] = toPhasedOp * rhs
  def unary_- : PhasedOp[M] = toPhasedOp * Phase.minusOne
}

object PhasedOpLike {

  implicit def toPhasedOp[M <: MonoidDef.Aux[M] with Singleton](x: PhasedOpLike[M]): PhasedOp[M] = x.toPhasedOp

}

/** An operator variable in a free monoid along with a phase. */
case class PhasedOp[M <: MonoidDef.Aux[M] with Singleton: Witness.Aux](phase: Phase, op: M#Op) extends PhasedOpLike[M] with MonoLike[M] with PolyLike[M] {
  lhs =>
  def M: M = valueOf[M]

  override def toString: String = Mono[M](phase, op).toString

  def toPhasedOp: PhasedOp[M] = lhs

  def toPoly: Poly[M, M] = Poly(lhs.toMono)

  def toMono: Mono[M, M] = Mono(lhs)

  override def *(newPhase: Phase)(implicit d: DummyImplicit): PhasedOp[M] = PhasedOp(phase * newPhase, op)
}

object PhasedOp {

  implicit def fromOp[M <: MonoidDef.Aux[M] with Singleton: Witness.Aux](op: M#Op): PhasedOp[M] = PhasedOp(Phase.one, op)

}
