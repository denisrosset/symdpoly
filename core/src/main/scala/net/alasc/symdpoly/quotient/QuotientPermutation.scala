package net.alasc.symdpoly.quotient

import cats.Invariant
import spire.algebra.Eq
import shapeless.Witness
import spire.algebra.{Action, Group}

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.evaluation.{EvaluatedMono2, Evaluator2}
import net.alasc.symdpoly.free.FreePermutation
import net.alasc.symdpoly.math.GenPerm
import net.alasc.symdpoly.{free, quotient, valueOf}
import shapeless.Witness
import spire.algebra.Action
import spire.syntax.group._

import net.alasc.symdpoly.{Mono, free, quotient, valueOf}
import cats.syntax.invariant._
import cats.syntax.contravariant._
import cats.instances.invariant._

import net.alasc.symdpoly.algebra.Instances._
import cats.instances.eq._

import net.alasc.symdpoly.generic.GenericPermutation

class QuotientPermutation[
  M <: quotient.MonoidDef with Singleton: Witness.Aux
](val genPerm: GenPerm) extends GenericPermutation[M] {
  def M: M = valueOf[M]
  def F: free.MonoidDef = valueOf[M].Free
  override def toString: String = FreePermutation.prettyPrintGenPerm(genPerm, F)
  override def hashCode: Int = genPerm.hashCode
  override def equals(any: Any): Boolean = any match {
    case that: QuotientPermutation[M] => (this.M eq that.M) && (this.genPerm == that.genPerm)
    case _ => false
  }
}

object QuotientPermutation {

  def applyUnsafe[M <: quotient.MonoidDef with Singleton:Witness.Aux](g: FreePermutation[M#Free]): QuotientPermutation[M] =
    new QuotientPermutation[M](g.genPerm)

  implicit def faithfulPermutationActionBuilder[M <: quotient.MonoidDef with Singleton]: FaithfulPermutationActionBuilder[QuotientPermutation[M]] =
    FaithfulPermutationActionBuilder[GenPerm].contramap(_.genPerm)

  implicit def equ[M <: quotient.MonoidDef with Singleton]: Eq[QuotientPermutation[M]] = Eq[GenPerm].contramap(_.genPerm)

  implicit def group[M <: quotient.MonoidDef with Singleton:Witness.Aux]: Group[QuotientPermutation[M]] = Group[GenPerm].imap(new QuotientPermutation[M](_))(_.genPerm)

  implicit def quotientMonoAction[M <: quotient.MonoidDef with Singleton:Witness.Aux]: Action[M#Monomial, QuotientPermutation[M]] = {
    val res = new QuotientMonoPermutationAction[M with quotient.MonoidDef.Aux[M#Free] with Singleton, M#Free]()(valueOf[M].witness.asInstanceOf[Witness.Aux[M with quotient.MonoidDef.Aux[M#Free] with Singleton]])
    res.asInstanceOf[Action[M#Monomial, QuotientPermutation[M]]]
  }

  implicit def evaluatedMonoAction[
    E <: Evaluator2[M] with Singleton: Witness.Aux,
    M <: quotient.MonoidDef with Singleton
  ](implicit action: Action[M#Monomial, QuotientPermutation[M]]): Action[EvaluatedMono2[E, M], QuotientPermutation[M]] =
    Invariant[Lambda[P => Action[P, QuotientPermutation[M]]]].imap[M#Monomial, EvaluatedMono2[E, M]](action)((mono: M#Monomial) => (valueOf[E]: E).apply(mono))(_.normalForm)

  implicit def grpQuotientPermutationOps[M <: quotient.MonoidDef with Singleton: Witness.Aux](grp: Grp[QuotientPermutation[M]]): GenericPermutation.GrpGenericPermutationOps[M, QuotientPermutation[M]] =
    new GenericPermutation.GrpGenericPermutationOps[M, QuotientPermutation[M]](grp)

}

class QuotientMonoPermutationAction[
M <: quotient.MonoidDef.Aux[F] with Singleton:Witness.Aux,
F <: free.MonoidDef.Aux[F] with Singleton
] extends Action[Mono[M, F], QuotientPermutation[M]] {
  def actl(g: QuotientPermutation[M], mono: Mono[M, F]): Mono[M, F] = actr(mono, g.inverse)
  def actr(mono: Mono[M, F], g: QuotientPermutation[M]): Mono[M, F] = {
    val word = mono.normalForm.mutableCopy.applyGenPermAction(g.genPerm)
    valueOf[M].inPlaceNormalForm(word)
    new Mono[M, F](word.setImmutable())
  }
}
