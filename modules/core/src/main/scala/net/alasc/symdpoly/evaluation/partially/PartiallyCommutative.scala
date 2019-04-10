package net.alasc.symdpoly.evaluation.partially

import scala.collection.compat._
import scala.collection.immutable.SortedSet

import cats.data.Validated
import cats.instances.either._
import cats.instances.partialOrder._
import cats.instances.vector._
import cats.syntax.alternative._
import cats.syntax.contravariant._
import shapeless.Witness
import spire.algebra.PartialOrder
import spire.algebra.lattice.{BoundedLattice, JoinSemilattice, MeetSemilattice}
import spire.syntax.order._

import net.alasc.partitions.Partition
import net.alasc.symdpoly.util.IndexMap
import net.alasc.symdpoly.{free, quotient, valueOf}

class OpPartition[F <: free.MonoidDef.Aux[F] with Singleton: Witness.Aux](val partition: Partition) {

  def toSetOfSets: Set[Set[F#Op]] = partition.blocks.map(_.map(i => valueOf[F].opFromIndex(i))).toSet

  def blockFor(op: F#Op): Set[F#Op] = partition.blockFor(op.index).map(i => valueOf[F].opFromIndex(i))

}

object OpPartition {

  def fromPartition[F <: free.MonoidDef.Aux[F] with Singleton: Witness.Aux](partition: Partition): OpPartition[F] =
    new OpPartition[F](partition)

  def apply[F <: free.MonoidDef.Aux[F] with Singleton: Witness.Aux](blocks: Set[Set[F#Op]]): OpPartition[F] =
    fromPartition(Partition.fromSortedBlocks(blocks.map(_.map(_.index).to(SortedSet)).toSeq.sortBy(_.min)))

  implicit def partialOrder[F <: free.MonoidDef.Aux[F] with Singleton]: PartialOrder[OpPartition[F]] =
    PartialOrder[Partition].contramap(_.partition)

  implicit def boundedLattice[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: BoundedLattice[OpPartition[F]] = new BoundedLattice[OpPartition[F]] {
    def zero: OpPartition[F] = new OpPartition[F](Partition.fromSortedBlocks(Seq.tabulate(valueOf[F].nOperators)(SortedSet(_))))
    def one: OpPartition[F] = new OpPartition[F](Partition.fromSortedBlocks(Seq((0 until valueOf[F].nOperators).to(SortedSet))))
    def meet(lhs: OpPartition[F], rhs: OpPartition[F]): OpPartition[F] = new OpPartition[F](MeetSemilattice[Partition].meet(lhs.partition, rhs.partition))
    def join(lhs: OpPartition[F], rhs: OpPartition[F]): OpPartition[F] = new OpPartition[F](JoinSemilattice[Partition].join(lhs.partition, rhs.partition))
  }

}

/** A quotient monoid is partially commutative if the following rules are obeyed:
  * - equivalence classes are defined among operators for operators that do *not* commute
  * - those equivalence classes are ordered by the commutation relations
  * - other rewriting rules stay inside an equivalence class.
  *
  * This class provides helper functions to investigate the partial commutation relations of a monoid.
  */
class PartiallyCommutative[
  M <: quotient.MonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] {

  def M: M = valueOf[M]
  def F: F = M.Free
  implicit def witnessF: Witness.Aux[F] = F.witness

  lazy val pair = {
    val res = M.rewritingRules.entries.toVector.map {
      case (lhs, rhs) if lhs.length == 2 && rhs.length == 2 && lhs(0) == rhs(1) && lhs(1) == rhs(0) =>
        Right((rhs(0), rhs(1)))
      case (lhs, rhs) => Left((0 until lhs.length).map(lhs(_)).toSet union (0 until rhs.length).map(rhs(_)).toSet)
    }.separate
    (res._1.toSet, res._2.toSet)
  }

  /** Pairs (op1, op2) appearing in a commutation rule that enforces op1 < op2. */
  def commutationRelations: Set[(F#Op, F#Op)] = pair._2

  // for each operator, the operators less than it according to the commutation rules
  lazy val lessThan: Map[F#Op, Set[F#Op]] = commutationRelations.groupBy(_._2).mapValues(_.map(_._1).toSet)

  // for each operator, the operators greater than it according to the commutation rules
  lazy val greaterThan: Map[F#Op, Set[F#Op]] = commutationRelations.groupBy(_._1).mapValues(_.map(_._2).toSet)

  /** Sets of operators entering in rewriting rules that are not of commutation type. */
  def opsets: Set[Set[F#Op]] = pair._1

  lazy val opsInOtherRewritings: Set[F#Op] = opsets.flatten

  lazy val orderedBlocks: Validated[String, IndexMap[Set[F#Op]]] = {
    // Finds by induction the commutation blocks and their ordering.
    // At any time, "largest" contains the largest blocks already found, in increasing order,
    // while "remainingLess" contains the operators not part of those blocks, in the keys and values
    def rec(remainingGreater: Map[F#Op, Set[F#Op]], largest: List[Set[F#Op]]): Validated[String, List[Set[F#Op]]] =
      if (remainingGreater.isEmpty) Validated.valid(largest) else {
        // Finds elements at the top, i.e. those who do not have any element greater than them
        val top = remainingGreater.keySet.filter(remainingGreater(_).isEmpty)
        // Verifies that all other elements have those elements greater than them
        val remainingGreater1 = remainingGreater.filterKeys(!top.contains(_))
        remainingGreater1.collectFirst {
          case (lhs, set) if (top diff set).nonEmpty => (lhs, top diff set)
        } match {
          case Some((lhs, rhsSet)) => Validated.invalid("Missing commutation relation " + rhsSet.map(rhs => s"$lhs < $rhs").mkString(", "))
          case None =>
            val remainingGreater2 = remainingGreater.mapValues(_ diff top)
            rec(remainingGreater2, top :: largest)
        }
    }
    val notPresent = F.allOperators.toSet diff greaterThan.keySet
    if (notPresent.nonEmpty)
      Validated.invalid(s"The operators in the set $notPresent are not part of any commutation relation")
    else
      rec(greaterThan, Nil).map(IndexMap(_))
  }

  lazy val partitionCommutation: Validated[String, OpPartition[F]] = orderedBlocks.map(im => OpPartition(im.elements.toSet))

  lazy val partitionOtherRewritings: OpPartition[F] = {
    val forest = net.alasc.partitions.algos.DisjointSetForest(F.nOperators)
    // we compute the partition of operators in equivalence classes
    // iterate over other rewriting rules, establish equivalence
    opsets.foreach { set =>
      val i = set.head
      for (j <- set if i != j) forest.union(i.index, j.index)
    }
    OpPartition.fromPartition[F](forest.toPartition)
  }

  /** The computed commutation partition of the quotient monoid. */
  lazy val partition: Validated[String, OpPartition[F]] =
    partitionCommutation.ensure("Rewriting rules are present between commutative blocks")(partitionOtherRewritings <= _)

  lazy val partitionMap: Validated[String, Map[Set[F#Op], Boolean]] =
    partition.map( opPartition => opPartition.toSetOfSets.map( set => (set, (set intersect opsInOtherRewritings).nonEmpty) ).toMap )

}

object PartiallyCommutative {
  def apply[F <: free.MonoidDef.Aux[F] with Singleton](M: quotient.MonoidDef.Aux[F]): PartiallyCommutative[M.type, F] =
    new PartiallyCommutative[M.type, F]
}
