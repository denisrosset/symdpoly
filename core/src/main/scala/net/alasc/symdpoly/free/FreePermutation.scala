package net.alasc.symdpoly
package free

import cats.{Contravariant, Invariant}
import shapeless.Witness
import cats.syntax.invariant._
import cats.syntax.contravariant._

import net.alasc.finite.FaithfulPermutationActionBuilder
import net.alasc.symdpoly.math.{GenPerm, PhasedInt}
import net.alasc.symdpoly.{Phase, free, valueOf}
import net.alasc.symdpoly.algebra.Instances._
import cats.instances.eq._
import spire.syntax.group._
import cats.instances.invariant._
import spire.algebra.{Action, Eq, Group}

class FreePermutation[F <: free.MonoidDef with Singleton:Witness.Aux](val genPerm: GenPerm) {
  def F: F = valueOf[F]
  override def toString: String = FreePermutation.prettyPrintGenPerm(genPerm, valueOf[F])
}

object FreePermutation {

  def prettyPrintGenPerm(opAction: GenPerm, F: MonoidDef): String = {
    val elements = for {
      i <- 0 until F.nOperators
      PhasedInt(phase, image) = opAction.image(PhasedInt(Phase.one, i)) if phase != Phase.one || i != image
      op = F.opFromIndex(i)
      opImage = F.PhasedOp(phase, F.opFromIndex(image))
    } yield s"$op -> $opImage"
    elements.mkString("{", ", ", "}")
  }

  // TODO: move this to free.MonoidDef

  implicit def faithfulPermutationActionBuilder[F <: free.MonoidDef with Singleton]: FaithfulPermutationActionBuilder[FreePermutation[F]] =
    FaithfulPermutationActionBuilder[GenPerm].contramap(_.genPerm)

  implicit def equ[F <: free.MonoidDef with Singleton]: Eq[FreePermutation[F]] = Eq[GenPerm].contramap(_.genPerm)

  implicit def group[F <: free.MonoidDef with Singleton:Witness.Aux]: Group[FreePermutation[F]] = Invariant[Group].imap(Group[GenPerm])(new FreePermutation[F](_))(_.genPerm)

  implicit def freeMonoAction[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Action[Mono[F, F], FreePermutation[F]] =  new FreeMonoOpPermutationAction[F]

}

class FreeMonoOpPermutationAction[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux] extends Action[Mono[F, F], FreePermutation[F]] {
  def actr(mono: Mono[F, F], g: FreePermutation[F]): Mono[F, F] = {
    val res = mono.mutableCopy
    res.applyGenPermAction(g.genPerm)
    new Mono[F, F](res.setImmutable())
  }

  def actl(g: FreePermutation[F], mono: Mono[F, F]): Mono[F, F] = actr(mono, g.inverse)
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

class QuotientPermutation[
  M <: quotient.MonoidDef with Singleton: Witness.Aux
](val genPerm: GenPerm) {
  def F: free.MonoidDef = valueOf[M].Free
  override def toString: String = FreePermutation.prettyPrintGenPerm(genPerm, F)
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
}
