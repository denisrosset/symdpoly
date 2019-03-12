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

  def invariantSubgroupOf[A:ClassTag:Order:Phased, G:ClassTag:Eq:FaithfulPermutationActionBuilder:Group](keys: Iterable[A], value: A => Cyclo, grp: Grp[G], cyclotomicOrder: Int)
                                                                                      (implicit action: Action[A, G]): Grp[G] = {
    val elements: OrderedSet[A] = allElementsUnderOrbit(keys, grp.generators, Phased[A].phaseCanonical(_))
    val monomialsAction: PermutationAction[G] = new PermutationAction[G] {
      def isFaithful: Boolean = false
      def findMovedPoint(g: G): NNOption = {
        cforRange(0 until elements.length * cyclotomicOrder) { i =>
          if (actr(i, g) != i) return NNSome(i)
        }
        NNNone
      }
      def movedPointsUpperBound(g: G): NNOption = NNSome(elements.length * cyclotomicOrder - 1)
      def actr(p: Int, g: G): Int = {
        val phase = Phase(p % cyclotomicOrder, cyclotomicOrder)
        val index = p / cyclotomicOrder
        val res = action.actr(elements(index), g)
        val canonical = res.phaseCanonical
        val newPhase = res.phaseOffset * phase
        val newIndex = elements.indexOf(canonical)
        newIndex * cyclotomicOrder + newPhase.numeratorIn(cyclotomicOrder)
      }
      def actl(g: G, p: Int): Int = actr(p, g.inverse)
    }
    val coeffSeq = for {
      element <- elements.iterator.toVector
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
