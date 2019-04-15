package net.alasc.symdpoly
package evaluation

import scala.collection.immutable.HashSet

import shapeless.Witness
import spire.syntax.action._
import spire.syntax.involution._
import spire.syntax.cfor._
import cats.syntax.contravariant._
import spire.syntax.ring._
import spire.syntax.order._
import syntax.phased._
import net.alasc.finite.Grp
import net.alasc.symdpoly.generic.SingleMoment
import net.alasc.symdpoly.math.{GrpDecomposition, Phase}
import spire.syntax.std.seq._

import net.alasc.perms.default._
import net.alasc.symdpoly.free.MutableWord
import scala.annotation.tailrec

import cats.Contravariant

import instances.invariant._
import net.alasc.algebra.PermutationAction
import net.alasc.perms.Perm
import net.alasc.symdpoly.evaluation.parts.{OpPartition, PartiallyCommutative}
import net.alasc.symdpoly.freebased.Permutation

/** Equivalence under the adjoint operation. */
class PPTEvaluator[
  M <: freebased.MonoDef.Aux[F] with Singleton,
  F <: free.MonoDef.Aux[F] with Singleton
](val partition: OpPartition[F], val symmetryGroup: Grp[M#PermutationType])(implicit val witnessMono: Witness.Aux[M]) extends Evaluator {

  implicit def witnessF: Witness.Aux[F] = valueOf[M].witnessFree
  def F: F = valueOf[F]

  type Mono = M

  def allTransposes(mono: M#MonoType): Set[M#MonoType] =
    if (mono.data.length <= 1) Set(mono) else {
      val freebased.Mono(phase: Phase, ops: Vector[F#Op]) = mono.normalForm
      val parts = ops.groupBy(op => partition.underlying.blockIndex(F.indexFromOp(op))).values.toList
      @tailrec def rec(lhs: Set[M#MonoType], rhs: List[Vector[F#Op]]): Set[M#MonoType] = rhs match {
        case hd :: tl =>
          val rmono = M.quotient(freebased.Mono.fromSeq(hd))
          rec(lhs.flatMap(l => HashSet(l * rmono, l * rmono.adjoint)), tl)
        case Nil => lhs
      }
      rec(HashSet(M.one * phase), parts)
    }

  def apply(mono: M#MonoType): SingleMomentType = {
    val start = allTransposes(mono)
    val candidates = symmetrizeMonoSet[M](start, symmetryGroup)
    fromNormalForm(findCanonicalInSet[M](candidates))
  }

  protected def buildWithSymmetryGroup(newSymmetryGroup: Grp[M#PermutationType]): Evaluator.Aux[M] =
    new PPTEvaluator[M, F](partition, newSymmetryGroup)

  private[this] val permAction: PermutationAction[M#PermutationType] = Contravariant[PermutationAction].contramap(Perm.algebra)(_.genPerm.perm)

  private[this] def predicate(p: Perm): Boolean = {
    val u = partition.underlying
    cforRange(0 until u.size) { i =>
      if (u.blockFor(i) != u.blockFor(i <|+| p)) return false
    }
    true
  }

  private[this] def backtrackTest(i: Int, j: Int): Boolean =
    partition.underlying.blockFor(i) == partition.underlying.blockFor(j)


  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] =
    grp.subgroupFor(permAction, backtrackTest, predicate)

  def isReal: Boolean = true

}

object PPTEvaluator {

  def apply[
    M <: quotient.MonoDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoDef.Aux[F] with Singleton
  ](partition: OpPartition[F], symmetryGroup: Grp[M#PermutationType]): PPTEvaluator[M, F] =
    PartiallyCommutative[F](valueOf[M]: M).partition.fold(err => sys.error(s"Invalid partially commutative monoid: $err"), parts => {
      require(partition >= parts, s"The PPT partition $partition should be equal or coarser than the commutative partition $parts")
      new PPTEvaluator[M, F](partition, symmetryGroup)
    })

}