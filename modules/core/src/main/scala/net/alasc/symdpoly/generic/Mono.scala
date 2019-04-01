package net.alasc.symdpoly
package generic

import scala.reflect.ClassTag

import cats.evidence.{As, Is}
import shapeless.Witness
import spire.algebra.{Action, Involution, Order}

import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import net.alasc.symdpoly.math.Phase

/** Mixin trait for types that can be converted to a M#MonoType for a monoid M.
  *
  * See [[PolyLike]] for the general logic.
  *
  * This trait defines all the methods where we are sure that the return type is M#MonoType.
  */
trait MonoLike[M <: MonoidDef with Singleton] extends PolyLike[M] { lhs =>
  def M: M

  // abstract method to implement
  def toMono: M#MonoType
  def **(rhs: Int): M#MonoType = lhs.pow(rhs)
  def pow(rhs: Int): M#MonoType = M.monoMultiplicativeBinoid.pow(lhs.toMono, rhs)
  def *(rhs: MonoLike[M]): M#MonoType = M.monoMultiplicativeBinoid.times(lhs.toMono, rhs.toMono)
}

object MonoLike {

  implicit def toMono[M <: MonoidDef with Singleton](monoLike: MonoLike[M]): M#MonoType = monoLike.toMono

}

abstract class Mono[M <: generic.MonoidDef with Singleton] extends MonoLike[M] with PolyLike[M] { lhs: M#MonoType =>
  def degree: Int
  def *(rhs: Phase)(implicit d: DummyImplicit): Mono[M] = M.monoPhased.gtimesr(lhs, rhs)
  def /(rhs: Phase)(implicit d: DummyImplicit): Mono[M] = lhs * rhs.reciprocal
}
