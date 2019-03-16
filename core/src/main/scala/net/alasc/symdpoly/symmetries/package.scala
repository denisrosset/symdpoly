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
    val canonicalElements: OrderedSet[A] = Orbit.allElements(keys, grp.generators, Phased[A].phaseCanonical(_))
    val monomialsAction: PermutationAction[G] = Actions.permutationAction(canonicalElements, cyclotomicOrder)
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
