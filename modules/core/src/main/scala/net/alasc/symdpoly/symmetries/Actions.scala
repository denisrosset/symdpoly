package net.alasc.symdpoly
package symmetries

import cyclo.Cyclo
import net.alasc.algebra.PermutationAction
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.partitions.Partition
import net.alasc.symdpoly.algebra.Phased
import util.OrderedSet
import shapeless.Witness
import spire.algebra.{Action, Eq, Group, Order}
import spire.syntax.cfor._

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.reflect.ClassTag
import spire.syntax.group._
import net.alasc.perms.default._
import net.alasc.symdpoly.math.Phase
import net.alasc.util._
import syntax.phased._

object Actions {

  /** Constructs the permutation action */
  def permutationActionOnSet[A:Order, G:Group](canonicalElements: OrderedSet[A])(implicit action: Action[A, G]): PermutationAction[G] =
    new PermutationAction[G] {
      def isFaithful: Boolean = false
      def findMovedPoint(g: G): NNOption = {
        cforRange(0 until canonicalElements.length) { i =>
          if (actr(i, g) != i) return NNSome(i)
        }
        NNNone
      }
      def movedPointsUpperBound(g: G): NNOption = NNSome(canonicalElements.length - 1)
      def actr(p: Int, g: G): Int = {
        val a = canonicalElements(p)
        val a1 = action.actr(a, g)
        canonicalElements.indexOf(a1)
      }
      def actl(g: G, p: Int): Int = actr(p, g.inverse)
    }

  /** Constructs the permutation action */
  def permutationAction[A:Order:Phased, G:Group](canonicalElements: OrderedSet[A], cyclotomicOrder: Int)(implicit action: Action[A, G]): PermutationAction[G] =
    new PermutationAction[G] {
      def isFaithful: Boolean = false
      def findMovedPoint(g: G): NNOption = {
        cforRange(0 until canonicalElements.length * cyclotomicOrder) { i =>
          if (actr(i, g) != i) return NNSome(i)
        }
        NNNone
      }
      def movedPointsUpperBound(g: G): NNOption = NNSome(canonicalElements.length * cyclotomicOrder - 1)
      def actr(p: Int, g: G): Int = {
        val phase = Phase(p % cyclotomicOrder, cyclotomicOrder)
        val index = p / cyclotomicOrder
        val res = action.actr(canonicalElements(index), g)
        val canonical = res.phaseCanonical
        val newPhase = res.phaseOffset * phase
        val newIndex = canonicalElements.indexOf(canonical)
        newIndex * cyclotomicOrder + newPhase.numeratorIn(cyclotomicOrder)
      }
      def actl(g: G, p: Int): Int = actr(p, g.inverse)
    }

}
