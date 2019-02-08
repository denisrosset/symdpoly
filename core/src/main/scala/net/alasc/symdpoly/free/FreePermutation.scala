package net.alasc.symdpoly
package free

import cats.{Contravariant, Invariant}
import shapeless.Witness
import cats.syntax.invariant._
import cats.syntax.contravariant._

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.math.{GenPerm, GenPermFaithfulPermutationAction, PhasedInt, Phases}
import net.alasc.symdpoly.{Phase, free, valueOf}
import net.alasc.symdpoly.algebra.Instances._
import cats.instances.eq._
import spire.syntax.group._
import cats.instances.invariant._
import spire.algebra.{Action, Eq, Group}

import cyclo.Cyclo

import net.alasc.partitions.Partition
import net.alasc.perms.Perm
import net.alasc.symdpoly
import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.algebra.Phased.syntax._
import net.alasc.symdpoly.evaluation._
import net.alasc.symdpoly.evaluation.Symmetries.{allEvaluatedMonomials, momentSetAction}
import net.alasc.symdpoly.generic.GenericPermutation
import net.alasc.symdpoly.quotient.QuotientPermutation

class FreePermutation[F <: free.MonoidDef with Singleton:Witness.Aux](val genPerm: GenPerm) extends GenericPermutation[F] {
  def F: F = valueOf[F]
  override def toString: String = FreePermutation.prettyPrintGenPerm(genPerm, valueOf[F])
  override def hashCode: Int = genPerm.hashCode
  override def equals(any: Any): Boolean = any match {
    case that: FreePermutation[F] => (this.F eq that.F) && (this.genPerm == that.genPerm)
    case _ => false
  }
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
