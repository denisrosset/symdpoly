package net.alasc.symdpoly.free

import cats.{Contravariant, Invariant}
import shapeless.Witness

import net.alasc.finite.FaithfulPermutationActionBuilder
import net.alasc.symdpoly.math.{GenPerm, PhasedInt}
import net.alasc.symdpoly.{Phase, free, valueOf}
import net.alasc.symdpoly.algebra.Instances._
import cats.instances.eq._
import cats.instances.invariant._
import spire.algebra.{Eq, Group}

class OpGenPerm[F <: free.MonoidDef with Singleton:Witness.Aux] protected[symdpoly](val opAction: GenPerm) {

  def F: F = valueOf[F]
  override def toString: String = OpGenPerm.prettyPrintGenPerm(opAction, valueOf[F])
}

object OpGenPerm {

  def prettyPrintGenPerm(opAction: GenPerm, F: MonoidDef): String = {
    val elements = for {
      i <- 0 until F.nOperators
      PhasedInt(phase, image) = opAction.image(PhasedInt(Phase.one, i)) if phase != Phase.one || i != image
      op = F.opFromIndex(i)
      opImage = F.PhasedOp(phase, F.opFromIndex(image))
    } yield s"$op -> $opImage"
    elements.mkString("{", ", ", "}")
  }

  implicit def faithfulPermutationActionBuilder[F <: free.MonoidDef with Singleton]: FaithfulPermutationActionBuilder[OpGenPerm[F]] =
    Contravariant[FaithfulPermutationActionBuilder].contramap(FaithfulPermutationActionBuilder[GenPerm])(_.opAction)

  implicit def equ[F <: free.MonoidDef with Singleton]: Eq[OpGenPerm[F]] = Contravariant[Eq].contramap(Eq[GenPerm])(_.opAction)

  implicit def group[F <: free.MonoidDef with Singleton:Witness.Aux]: Group[OpGenPerm[F]] = Invariant[Group].imap(Group[GenPerm])(new OpGenPerm[F](_))(_.opAction)

}
