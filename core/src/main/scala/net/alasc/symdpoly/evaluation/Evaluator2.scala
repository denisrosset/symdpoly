package net.alasc.symdpoly.evaluation

import cats.{Contravariant, Invariant}
import shapeless.Witness
import spire.algebra.{Eq, Order, VectorSpace}

import cyclo.Cyclo

import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.generic
import cats.instances.order.catsContravariantMonoidalForOrder
import cats.instances.eq.catsContravariantMonoidalForEq
import net.alasc.symdpoly.algebra.Instances._

trait Evaluator2[M <: generic.MonoidDef with Singleton] { self =>

  val witness: Witness.Aux[self.type] = Witness.mkWitness(self)

  def M: M

  // optimization: set to true if apply(a) == apply(a.adjoint)
  def isSelfAdjoint: Boolean

  def apply(mono: M#Monomial): EvaluatedMono2[self.type, M]

  def apply(poly: M#Polynomial)(implicit d: DummyImplicit): EvaluatedPoly2[self.type, M]

  // typeclasses

  val evaluatedMonoOrder: Order[EvaluatedMono2[self.type, M]] = Contravariant[Order].contramap(M.monoOrder)(em => em.normalForm)
  val evaluatedMonoPhased: Phased[EvaluatedMono2[self.type, M]] = Invariant[Phased].imap(M.monoPhased)(apply)(_.normalForm)
  val evaluatedPolyEq: Eq[EvaluatedPoly2[self.type, M]] = Contravariant[Eq].contramap(M.polyEq)(ep => ep.normalForm)
  val evaluatedPolyVectorSpace: VectorSpace[EvaluatedPoly2[self.type, M], Cyclo] = Invariant[Lambda[V => VectorSpace[V, Cyclo]]].imap(M.polyAssociativeAlgebra)(apply)(_.normalForm)

}
