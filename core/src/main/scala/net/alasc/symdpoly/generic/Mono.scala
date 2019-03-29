package net.alasc.symdpoly
package generic

import scala.reflect.ClassTag

import cats.evidence.{As, Is}
import shapeless.Witness
import spire.algebra.{Action, Involution, Order}

import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}

abstract class Mono[M <: generic.MonoidDef with Singleton:Witness.Aux] { self: M#MonoType =>
  def toPoly: M#PolyType
  def M: M = valueOf[M]
  def degree: Int
}

/* TODO: remove if unused
object Mono {

  implicit def involution[M <: generic.MonoidDef with Singleton:Witness.Aux]: Involution[Mono[M]] = valueOf[M].monoInvolution
  implicit def multiplicativeBinoid[M <: generic.MonoidDef with Singleton:Witness.Aux]: MultiplicativeBinoid[Mono[M]] = valueOf[M].monoMultiplicativeBinoid
  implicit def order[M <: generic.MonoidDef with Singleton:Witness.Aux]: Order[Mono[M]] = valueOf[M].monoOrder
  implicit def classTag[M <: generic.MonoidDef with Singleton:Witness.Aux]: ClassTag[Mono[M]] = valueOf[M].monoClassTag
  implicit def genPermAction[M <: generic.MonoidDef with Singleton:Witness.Aux]: Action[Mono[M], Permutation[M]] = valueOf[M].permutationMonoAction
  implicit def phased[M <: generic.MonoidDef with Singleton:Witness.Aux]: Phased[Mono[M]] = valueOf[M].monoPhased

}

 */
