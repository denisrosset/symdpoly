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

object Orbit {

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

}
