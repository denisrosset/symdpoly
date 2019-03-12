package net.alasc.symdpoly
package generic

import scala.reflect.ClassTag
import cats.Invariant
import shapeless.Witness
import spire.algebra.{Action, Eq, Group, Order}
import net.alasc.algebra.PermutationAction
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.algebra.Phased
import net.alasc.util._
import syntax.all._
import instances.all._
import spire.syntax.cfor._
import spire.syntax.action._
import spire.syntax.group._
import cats.syntax.invariant._
import cats.syntax.contravariant._
import net.alasc.partitions.Partition
import net.alasc.symdpoly.math.{Phase, PhasedInt}
import net.alasc.perms.default._
import net.alasc.symdpoly.util.OrderedSet

/** A Permutation relabels the operator variables of monomials, possibly with a phase. */
trait Permutation[M <: generic.MonoidDef with Singleton] { self: M#Permutation =>

}

object Permutation {


  def phasedIntAction[
    M <: generic.MonoidDef with Singleton:Witness.Aux
  ](set: OrderedSet[M#Monomial])(implicit group: Group[Permutation[M]],
                                 action: Action[M#Monomial, Permutation[M]]): Action[PhasedInt, Permutation[M]] =
    new Action[PhasedInt, Permutation[M]] {
      def actr(p: PhasedInt, g: Permutation[M]): PhasedInt = {
        implicit def phased: Phased[M#Monomial] = valueOf[M].monoPhased
        implicit def order: Order[M#Monomial] = valueOf[M].monoOrder
        val res = set(p.index) <|+| g
        val newIndex = set.indexOf(res.phaseCanonical)
        val newPhase = p.phase * res.phaseOffset
        PhasedInt(newPhase, newIndex)
      }
      def actl(g: Permutation[M], p: PhasedInt): PhasedInt = actr(p, group.inverse(g))
    }

  implicit def evaluatedMonoAction[
    E <: Evaluator[M] with Singleton: Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ](implicit action: Action[M#Monomial, Permutation[M]]): Action[EvaluatedMono[E, M], Permutation[M]] =
    Invariant[Lambda[P => Action[P, Permutation[M]]]].imap[M#Monomial, EvaluatedMono[E, M]](action)((mono: M#Monomial) => (valueOf[E]: E).apply(mono))(_.normalForm)

}
