package net.alasc.symdpoly

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

package object symmetries {

  def allElementsUnderOrbit[A:ClassTag:Order, G](elements: Iterable[A], generators: Seq[G], normalForm: (A => A) = identity[A](_))
                                                (implicit action: Action[A, G]): OrderedSet[A] = {
    import scala.collection.mutable.HashSet
    val orbit: HashSet[A] = elements.to[HashSet]
    var inspect0: HashSet[A] = elements.to[HashSet]
    var newElements0: HashSet[A] = HashSet.empty[A]
    @tailrec def rec(inspect: HashSet[A], newElements: HashSet[A]): Unit =
      if (inspect.nonEmpty) {
        val it = inspect.iterator
        while (it.hasNext) {
          val a = it.next()
          cforRange(0 until generators.length) { i =>
            val g = generators(i)
            val ag = normalForm(action.actr(a, g))
            if (!orbit.contains(ag)) {
              orbit += ag
              newElements += ag
            }
          }
        }
        inspect.clear()
        rec(newElements, inspect)
      }
    rec(elements.to[HashSet], HashSet.empty[A])
    val array = orbit.toArray
    spire.math.Sorting.quickSort(array)
    new OrderedSet(array.map(_.asInstanceOf[AnyRef]))
  }

  /** Constructs the permutation action */
  def canonicalPermutationAction[A:Order:Phased, G:Group](canonicalElements: OrderedSet[A])(implicit action: Action[A, G]): PermutationAction[G] =
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

  /** Computes the subgroup of "grp" that leaves an element of a vector space over Cyclo invariant.
    *
    * @param keys  Basis elements that have nonzero coefficients in their canonical form.
    * @param value Value function that takes a basis element (canonical form) and returns its coefficient.
    * @param grp   Group to find a subgroup of.
    * @param cyclotomicOrder LCM of the cyclotomic orders present in the coefficients and the group action.
    * @param action Group action.
    * @tparam A Type of the basis elements of the vector space. Can include a phase.
    * @tparam G Group element type.
    * @return The invariant subgroup.
    */
  def invariantSubgroupOf[A:ClassTag:Order:Phased, G:ClassTag:Eq:FaithfulPermutationActionBuilder:Group](keys: Iterable[A], value: A => Cyclo, grp: Grp[G], cyclotomicOrder: Int)
                                                                                      (implicit action: Action[A, G]): Grp[G] = {
    val canonicalElements: OrderedSet[A] = allElementsUnderOrbit(keys, grp.generators, Phased[A].phaseCanonical(_))
    val monomialsAction: PermutationAction[G] = permutationAction(canonicalElements, cyclotomicOrder)
    val coeffSeq = for {
      element <- canonicalElements.iterator.toVector
      coeff = value(element)
      k <- 0 until cyclotomicOrder
      phase = Phase(k, cyclotomicOrder)
    } yield coeff * phase.toCyclo
    val partition = Partition.fromSeq(coeffSeq)
    val stabilizer = grp.orderedPartitionStabilizer(monomialsAction, partition)
    val generators = stabilizer.smallGeneratingSet
    Grp.fromGeneratorsAndOrder(generators, stabilizer.order)
  }

}
