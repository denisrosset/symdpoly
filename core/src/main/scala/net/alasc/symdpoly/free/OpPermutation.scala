package net.alasc.symdpoly
package free

import cats.{Contravariant, Invariant}
import shapeless.Witness

import net.alasc.finite.FaithfulPermutationActionBuilder
import net.alasc.symdpoly.math.{GenPerm, PhasedInt}
import net.alasc.symdpoly.{Phase, free, valueOf}
import net.alasc.symdpoly.algebra.Instances._
import cats.instances.eq._
import spire.syntax.group._
import cats.instances.invariant._
import spire.algebra.{Action, Eq, Group}

class OpPermutation[F <: free.MonoidDef with Singleton:Witness.Aux](val genPerm: GenPerm) {

  def F: F = valueOf[F]
  override def toString: String = OpPermutation.prettyPrintGenPerm(genPerm, valueOf[F])
}

object OpPermutation {

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

  implicit def faithfulPermutationActionBuilder[F <: free.MonoidDef with Singleton]: FaithfulPermutationActionBuilder[OpPermutation[F]] =
    Contravariant[FaithfulPermutationActionBuilder].contramap(FaithfulPermutationActionBuilder[GenPerm])(_.genPerm)

  implicit def equ[F <: free.MonoidDef with Singleton]: Eq[OpPermutation[F]] = Contravariant[Eq].contramap(Eq[GenPerm])(_.genPerm)

  implicit def group[F <: free.MonoidDef with Singleton:Witness.Aux]: Group[OpPermutation[F]] = Invariant[Group].imap(Group[GenPerm])(new OpPermutation[F](_))(_.genPerm)

  implicit def freeMonoAction[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Action[Mono[F, F], OpPermutation[F]] =  new FreeMonoOpPermutationAction[F]

}

class FreeMonoOpPermutationAction[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux] extends Action[Mono[F, F], OpPermutation[F]] {
  def actr(mono: Mono[F, F], g: OpPermutation[F]): Mono[F, F] = {
    val res = mono.mutableCopy
    res.applyGenPermAction(g.genPerm)
    new Mono[F, F](res.setImmutable())
  }

  def actl(g: OpPermutation[F], mono: Mono[F, F]): Mono[F, F] = actr(mono, g.inverse)
}
