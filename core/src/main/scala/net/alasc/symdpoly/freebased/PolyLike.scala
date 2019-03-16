package net.alasc.symdpoly
package freebased

import cats.evidence.Is
import shapeless.Witness
import spire.math.Rational

import cyclo.Cyclo

import net.alasc.symdpoly.math.Phase

/** Mixin trait offering methods that treat other objects as polynomials. */
trait PolyLike[M <: MonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton] { lhs =>

  // Abstract method to be implemented
  def toPoly: Poly[M, F]
  def +(rhs: Poly[M, F]): Poly[M, F]
  def *(rhs: Poly[M, F]): Poly[M, F]

  // +/- scalar
  def +(rhs: Int)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + Cyclo(rhs)
  def +(rhs: Rational)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + (rhs: Cyclo)
  def +(rhs: Phase)(implicit d: DummyImplicit, wM: Witness.Aux[M]): Poly[M, F] = lhs + (rhs: Cyclo)
  def +(rhs: Cyclo)(implicit wM: Witness.Aux[M]): Poly[M, F] =
    if (rhs.isZero) toPoly else lhs + Poly.constant[M, F](rhs)

  def -(rhs: Int)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + (-rhs)
  def -(rhs: Rational)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + (-rhs)
  def -(rhs: Phase)(implicit d: DummyImplicit, wM: Witness.Aux[M]): Poly[M, F] = lhs + (-rhs)
  def -(rhs: Cyclo)(implicit wM: Witness.Aux[M]): Poly[M, F] =
    if (rhs.isZero) toPoly else lhs + (-rhs)

  // +/- op

  def +(rhs: F#Op)(implicit wM: Witness.Aux[M], ev: Mono[F, F] Is Mono[M, F]): Poly[M, F] = lhs + ev.coerce(rhs.toMono)
  def -(rhs: F#Op)(implicit wM: Witness.Aux[M], ev: Mono[F, F] Is Mono[M, F]): Poly[M, F] = lhs - ev.coerce(rhs.toMono)

  // +/- phasedOp
  def +(rhs: F#PhasedOp)(implicit wM: Witness.Aux[M], ev: Mono[F, F] Is Mono[M, F]): Poly[M, F] = lhs + ev.coerce(rhs.toMono)
  def -(rhs: F#PhasedOp)(implicit wM: Witness.Aux[M], ev: Mono[F, F] Is Mono[M, F]): Poly[M, F] = lhs - ev.coerce(rhs.toMono)

  // +/- mono

  def +(rhs: Mono[M, F])(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + rhs.toPoly
  def -(rhs: Mono[M, F])(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + (-rhs).toPoly

  // * / scalar

  def *(rhs: Int)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * Cyclo(rhs)
  def *(rhs: Rational)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * (rhs: Cyclo)
  def *(rhs: Cyclo)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * Poly.constant[M, F](rhs)

  def /(rhs: Int)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * Cyclo(rhs).reciprocal
  def /(rhs: Rational)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * rhs.reciprocal
  def /(rhs: Cyclo)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * rhs.reciprocal

  def <=!(rhs: PolyLike[M, F])(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = OperatorConstraint[M](lhs.toPoly, ComparisonOp.LE, rhs.toPoly)
  def <=! (rhs: Int)(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = lhs <=! valueOf[M].constant(rhs)
  def <=! (rhs: Rational)(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = lhs <=! valueOf[M].constant(rhs)
  def <=! (rhs: Cyclo)(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = lhs <=! valueOf[M].constant(rhs)

  def >=!(rhs: PolyLike[M, F])(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = OperatorConstraint[M](lhs.toPoly, ComparisonOp.GE, rhs.toPoly)
  def >=! (rhs: Int)(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = lhs >=! valueOf[M].constant(rhs)
  def >=! (rhs: Rational)(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = lhs >=! valueOf[M].constant(rhs)
  def >=! (rhs: Cyclo)(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = lhs >=! valueOf[M].constant(rhs)

  def =!(rhs: PolyLike[M, F])(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = OperatorConstraint[M](lhs.toPoly, ComparisonOp.EQ, rhs.toPoly)
  def =! (rhs: Int)(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = lhs =! valueOf[M].constant(rhs)
  def =! (rhs: Rational)(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = lhs =! valueOf[M].constant(rhs)
  def =! (rhs: Cyclo)(implicit wM: Witness.Aux[M]): OperatorConstraint[M] = lhs =! valueOf[M].constant(rhs)

}
