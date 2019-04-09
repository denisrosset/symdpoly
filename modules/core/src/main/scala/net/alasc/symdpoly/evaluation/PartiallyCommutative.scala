package net.alasc.symdpoly
package evaluation

import cats.instances.vector._
import cats.instances.option._
import cats.instances.either._
import cats.syntax.traverse._
import cats.syntax.alternative._
import scala.annotation.tailrec

import shapeless.Witness

import net.alasc.partitions.Partition
import net.alasc.symdpoly.quotient.MonoidDef
import scala.collection.compat._
import scala.collection.immutable.SortedSet

/** A quotient monoid is partially commutative if the following rules are obeyed:
  * - equivalence classes are defined among operators for operators that do *not* commute
  * - those equivalence classes are ordered by the commutation relations
  * - other rewriting rules stay inside an equivalence class
  *
  * @param parts First, the keys form a set of equivalence classes. The associated value
  *              tells whether additional rewriting rules are present.
  */
case class PartiallyCommutative[
  M <: MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](parts: Map[Set[F#Op], Boolean])

object PartiallyCommutative {

  class InvestigatedQuotient[
    M <: quotient.MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ] {

    def M: M = valueOf[M]
    def F: F = M.Free
    implicit def witnessF: Witness.Aux[F] = F.witness

    // collect either the ordering behind a commutation relation (Right), or a set of operators entering another type of constraints (Left)
    val (opsets, relations) = M.rewritingRules.entries.toVector.map {
      case (lhs, rhs) if lhs.length == 2 && rhs.length == 2 && lhs(0) == rhs(1) && lhs(1) == rhs(0) =>
        Right((rhs(0).index, rhs(1).index))
      case (lhs, rhs) => Left((0 until lhs.length).map(lhs(_).index).toSet union (0 until rhs.length).map(rhs(_).index).toSet)
    }.separate

    // for each operator, the operators less than it
    val lessThan = relations.groupBy(_._2).mapValues(_.map(_._1).toSet)

    // for each operator, the operators greater than it
    val greaterThan = relations.groupBy(_._1).mapValues(_.map(_._2).toSet)

    def ansatzFromRules: Set[Set[F#Op]] = {
      // we compute the partition of operators in equivalence classes
      val forest = net.alasc.partitions.algos.DisjointSetForest(F.nOperators)
      // iterate over other rewriting rules, establish equivalence
      opsets.foreach { set =>
        val i = set.head
        for (j <- set if i != j) forest.union(i, j)
      }
      // iterate over operators, establish equivalence over operators that do not commute
      (0 until F.nOperators).foreach { i =>
        val set = (0 until F.nOperators).toSet diff (lessThan.getOrElse(i, Set.empty) union greaterThan.getOrElse(i, Set.empty))
        if (set.size > 1) {
          val i = set.head
          for (j <- set if i != j) forest.union(i, j)
        }
      }
      forest.toPartition.blocks.map(_.map(F.opIndexMap.elements)).toSet
    }

    def partitionFromSetOfSets(blocks: Set[Set[Int]]): Partition =
      Partition.fromSortedBlocks(blocks.map(_.to(SortedSet)).toSeq.sortBy(_.min))

    def verify(blocks: Set[Set[F#Op]]): Boolean = {
      val partition = partitionFromSetOfSets(blocks.map(_.map(_.index)))
      // verify that commutation relations are consistent with the discovered partition
      for (block <- partition.blocks) {
        val h = block.head
        @tailrec def checkBlocks(remaining: Set[Int]): Boolean =
          if (remaining.isEmpty) true else {
            val o = remaining.head
            val oBlock = partition.blockFor(o)
            if ((oBlock diff remaining).nonEmpty)
              false
            else
              checkBlocks(remaining diff oBlock)
          }
        if (!checkBlocks(lessThan.getOrElse(h, Set.empty))) return false
        if (!checkBlocks(greaterThan.getOrElse(h, Set.empty))) return false
      }
      true
    }

    def computeMap(blocks: Set[Set[F#Op]]): Map[Set[F#Op], Boolean] =
      blocks.map { (block: Set[F#Op]) =>
        (block, block.exists(i => opsets.exists(_.contains(i.index))))
      }.toMap

  }

  def compute[F <: free.MonoidDef.Aux[F] with Singleton](M: MonoidDef.Aux[F] with Singleton): Option[PartiallyCommutative[M.type, F]] = {
    implicit def witnessM: Witness.Aux[M.type] = M.witness
    def F: F = M.Free
    implicit def witnessF: Witness.Aux[F] = F.witness
    // collect either the ordering behind a commutation relation (Right), or a set of operators entering another type of constraints (Left)
    val (opsets, relations) = M.rewritingRules.entries.toVector.map {
      case (lhs, rhs) if lhs.length == 2 && rhs.length == 2 && lhs(0) == rhs(1) && lhs(1) == rhs(0) =>
        Right((rhs(0).index, rhs(1).index))
      case (lhs, rhs) => Left((0 until lhs.length).map(lhs(_).index).toSet union (0 until rhs.length).map(rhs(_).index).toSet)
    }.separate
    // now we compute the partition of operators in equivalence classes
    val forest = net.alasc.partitions.algos.DisjointSetForest(F.nOperators)
    // for each operator, the operators less than it
    val lessThan = relations.groupBy(_._2).mapValues(_.map(_._1).toSet)
    // for each operator, the operators greater than it
    val greaterThan = relations.groupBy(_._1).mapValues(_.map(_._2).toSet)
    // iterate over other rewriting rules, establish equivalence
    opsets.foreach { set =>
      val i = set.head
      for (j <- set if i != j) forest.union(i, j)
    }
    // iterate over operators, establish equivalence over operators that do not commute
    (0 until F.nOperators).foreach { i =>
      val set = (0 until F.nOperators).toSet diff (lessThan.getOrElse(i, Set.empty) union greaterThan.getOrElse(i, Set.empty))
      if (set.size > 1) {
        val i = set.head
        for (j <- set if i != j) forest.union(i, j)
      }
    }
    val partition = forest.toPartition
    // verify that commutation relations are consistent with the discovered partition
    for (block <- partition.blocks) {
      val h = block.head
      @tailrec def checkBlocks(remaining: Set[Int]): Boolean =
        if (remaining.isEmpty) true else {
          val o = remaining.head
          val oBlock = partition.blockFor(o)
          if ((oBlock diff remaining).nonEmpty)
            false
          else
            checkBlocks(remaining diff oBlock)
        }
      if (!checkBlocks(lessThan.getOrElse(h, Set.empty))) return None
      if (!checkBlocks(greaterThan.getOrElse(h, Set.empty))) return None
    }
    val blocks: Set[Set[F#Op]] = partition.blocks.toSet.map((block: partition.Block) => block.map((i: Int) => F.opIndexMap.elements(i)))
    val map = blocks.map { (block: Set[F#Op]) =>
      (block, block.exists(i => opsets.exists(_.contains(i.index))))
    }.toMap
    Some(PartiallyCommutative(map))
  }
}
