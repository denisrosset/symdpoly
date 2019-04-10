package net.alasc.symdpoly
package evaluation
package partially

import cats.Contravariant
import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.perms.Perm
import net.alasc.symdpoly.freebased.{Mono, Permutation}
import shapeless.Witness
import net.alasc.perms.default._

/*
/** Equivalence under transposition of the groups of operators.
  *
  * @param groupIndex -1 if cannot be transposed, other index of the transpose group
  */
final class TransposesEquivalence[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](groupIndex: Array[Int])(implicit val witnessM: Witness.Aux[M]) extends Equivalence[M] with FreeBasedComponent[M, F] {

  val components = groupIndex.distinct.toSeq.filterNot(_ == -1).map { g =>
    Component.transpose((op: F#Op) => groupIndex(F.indexFromOp(op)) == g)
  }

  val untransposed: Set[Int] = groupIndex.zipWithIndex.filter(_._1 == -1).map(_._2).toSet
  val partition: Partition = Partition.fromSeq(groupIndex)

  private[this] val action: PermutationAction[M#PermutationType] =
    instances.invariant.symdpolyContravariantForPermutationAction
      .contramap[Perm, M#PermutationType](Perm.algebra)(_.genPerm.perm)

  def apply(mono: M#MonoType): Set[M#MonoType] =
    components.foldLeft(Set(mono)) {
      case (res, equiv) => res.flatMap(m => equiv.apply(m))
    }

  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] =
    grp.setwiseStabilizer(action, untransposed).unorderedPartitionStabilizer(action, partition)

  def isReal: Boolean = untransposed.isEmpty

}
*/
