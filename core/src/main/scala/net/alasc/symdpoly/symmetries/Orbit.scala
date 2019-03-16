package net.alasc.symdpoly
package symmetries
import cyclo.Cyclo

import net.alasc.algebra.PermutationAction
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp, GrpPermutationAction}
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
import spire.syntax.order._

import net.alasc.perms.default._
import net.alasc.symdpoly.math.Phase
import net.alasc.util._

object Orbit {

  /** Checks whether the given groups has an action compatible with the given set of elements.
    *
    * In essence, returns whether elements == allElements(elements, generators, normalForm).
    */
  def compatible[A:Order, G](elements: Iterable[A], generators: Seq[G], normalForm: (A => A) = identity[A](_))
                                     (implicit action: Action[A, G]): Boolean = {
    import scala.collection.mutable.HashSet
    val orbit: HashSet[A] = elements.to[HashSet]
    @tailrec def rec(it: Iterator[A]): Boolean =
      if (!it.hasNext) true else {
        val a = it.next()
        cforRange(0 until generators.length) { i =>
          val g = generators(i)
          val img = normalForm(action.actr(a, g))
          if (!orbit.contains(img)) return false
        }
        rec(it)
      }
    rec(elements.iterator)
  }

  /** Enumerates the elements of an orbit, with possible reduction of elements to their normal form. */
  def allElements[A:ClassTag:Order, G](elements: Iterable[A], generators: Seq[G], normalForm: (A => A) = identity[A](_))
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

  def compatibleSubgroup[A:ClassTag:Order, G:GrpPermutationAction](elements: Iterable[A], grp: Grp[G], normalForm: (A => A) = ((x: A) => x))
                                             (implicit action: Action[A, G]): Grp[G] =
    if (compatible(elements, grp.generators, normalForm)) grp else {
    import grp.group
    val all = allElements(elements, grp.generators, normalForm)
    val permutationAction = new PermutationAction[G] {
      def isFaithful: Boolean = false
      def findMovedPoint(g: G): NNOption = {
        cforRange(0 until all.length) { i =>
          if (actr(i, g) != i) return NNSome(i)
        }
        NNNone
      }
      def movedPointsUpperBound(g: G): NNOption = if (all.length > 1) NNSome(all.length - 1) else NNNone
      def actl(g: G, p: Int): Int = actr(p, g.inverse)
      def actr(p: Int, g: G): Int = {
        val res = all.indexOf(normalForm(action.actr(all(p), g)))
        assert(res >= 0)
        res
      }
    }
    val originalSet = elements.toSet
    val subset = (0 until all.length).filter(i => originalSet.contains(all(i))).toSet
    grp.setwiseStabilizer(permutationAction, subset)
  }

}
