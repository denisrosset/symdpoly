package net.alasc.symdpoly
package evaluation
package internal

import cats.Contravariant
import shapeless.Witness

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.symdpoly.evaluation.internal.{AdjointEquivalence, ComposedEquivalence, CyclicEquivalence, TransposeEquivalence}
import instances.invariant._
import net.alasc.perms.default._
import net.alasc.perms.Perm

/** Equivalence under transposition of the groups of operators specified by the given group predicate */
final case class TransposesEquivalence[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](group: OpGroup[F])(implicit val witnessM: Witness.Aux[M]) extends FreeBasedEquivalence[M, F] {
  /*implicit def ct = M.permutationClassTag
  implicit def equ = M.permutationEq
  implicit def fpab = M.permutationFaithfulPermutationActionBuilder
  implicit def group = M.permutationGroup*/

  val groupIndices: Seq[Int] = F.opIndexMap.elements.flatMap(group(_)).toSet.toSeq
  val transposes = groupIndices.map(g => transpose[M, F](op => (group(op) == Some(g))))
  val partition = Partition.fromSeq(F.opIndexMap.elements.map(group(_)))
  val unassigned = F.opIndexMap.elements.map(group(_)).zipWithIndex.collect {
    case (None, i) => i
  }.toSet

  def apply(mono: M#Monomial): Set[M#Monomial] =
    transposes.foldLeft(Set(mono)) {
      case (res, equiv) => res.flatMap(m => equiv.apply(m))
    }

  def compatibleSubgroup(grp: Grp[M#Permutation]): Grp[M#Permutation] = {
    val action: PermutationAction[M#Permutation] = Contravariant[PermutationAction].contramap(Perm.algebra)(_.genPerm.perm)
    grp.setwiseStabilizer(action, unassigned).unorderedPartitionStabilizer(action, partition)
  }

  def isSelfAdjoint: Boolean = false
}
