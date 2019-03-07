package net.alasc.symdpoly
package generic

import cats.evidence.Is
import cyclo.Cyclo
import net.alasc.symdpoly.math.Phase
import shapeless.Witness
import spire.math.Rational

/** Mixin trait offering methods that treat other objects as polynomials. */
trait FreeBasedPolyTerm[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton] { lhs =>

  // Abstract method to be implemented
  def toPoly: FreeBasedPoly[M, F]
  def +(rhs: FreeBasedPoly[M, F]): FreeBasedPoly[M, F]
  def *(rhs: FreeBasedPoly[M, F]): FreeBasedPoly[M, F]

  // +/- scalar
  def +(rhs: Int)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs + (rhs: Cyclo)
  def +(rhs: Rational)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs + (rhs: Cyclo)
  def +(rhs: Phase)(implicit d: DummyImplicit, wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs + (rhs: Cyclo)
  def +(rhs: Cyclo)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] =
    if (rhs.isZero) toPoly else lhs + FreeBasedPoly.constant[M, F](rhs)

  def -(rhs: Int)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs + (-rhs)
  def -(rhs: Rational)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs + (-rhs)
  def -(rhs: Phase)(implicit d: DummyImplicit, wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs + (-rhs)
  def -(rhs: Cyclo)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] =
    if (rhs.isZero) toPoly else lhs + (-rhs)

  // +/- op

  def +(rhs: F#Op)(implicit wM: Witness.Aux[M], ev: FreeBasedMono[F, F] Is FreeBasedMono[M, F]): FreeBasedPoly[M, F] = lhs + ev.coerce(rhs.toMono)
  def -(rhs: F#Op)(implicit wM: Witness.Aux[M], ev: FreeBasedMono[F, F] Is FreeBasedMono[M, F]): FreeBasedPoly[M, F] = lhs - ev.coerce(rhs.toMono)

  // +/- phasedOp
  def +(rhs: F#PhasedOp)(implicit wM: Witness.Aux[M], ev: FreeBasedMono[F, F] Is FreeBasedMono[M, F]): FreeBasedPoly[M, F] = lhs + ev.coerce(rhs.toMono)
  def -(rhs: F#PhasedOp)(implicit wM: Witness.Aux[M], ev: FreeBasedMono[F, F] Is FreeBasedMono[M, F]): FreeBasedPoly[M, F] = lhs - ev.coerce(rhs.toMono)

  // +/- mono

  def +(rhs: FreeBasedMono[M, F])(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs + rhs.toPoly
  def -(rhs: FreeBasedMono[M, F])(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs + (-rhs).toPoly

  // * / scalar

  def *(rhs: Int)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs * (rhs: Cyclo)
  def *(rhs: Rational)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs * (rhs: Cyclo)
  def *(rhs: Cyclo)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs * FreeBasedPoly.constant[M, F](rhs)

  def /(rhs: Int)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs * Cyclo(rhs).reciprocal
  def /(rhs: Rational)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs * rhs.reciprocal
  def /(rhs: Cyclo)(implicit wM: Witness.Aux[M]): FreeBasedPoly[M, F] = lhs * rhs.reciprocal
}
