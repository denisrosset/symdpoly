package net.alasc.symdpoly
package evaluation.parts

import scala.collection.immutable.SortedSet
import scala.collection.compat._

import spire.algebra.PartialOrder
import spire.algebra.lattice._
import shapeless.Witness
import cats.instances.either._
import cats.instances.partialOrder._
import cats.instances.vector._
import cats.syntax.alternative._
import cats.syntax.contravariant._

import net.alasc.partitions.Partition

case class OpPartition[F <: free.MonoidDef.Aux[F] with Singleton: Witness.Aux](val underlying: Partition) {

  def F: F = valueOf[F]

  override def toString: String = underlying.blocks.map(block => block.toSeq.map(F.opFromIndex).mkString("[", " ", "]") ).mkString

  def toSetOfSets: Set[Set[F#Op]] = underlying.blocks.map(_.map(i => valueOf[F].opFromIndex(i))).toSet

  def blockFor(op: F#Op): Set[F#Op] = underlying.blockFor(op.index).map(i => valueOf[F].opFromIndex(i))

}

object OpPartition {

  def fromPartition[F <: free.MonoidDef.Aux[F] with Singleton: Witness.Aux](partition: Partition): OpPartition[F] =
    new OpPartition[F](partition)

  def apply[F <: free.MonoidDef.Aux[F] with Singleton: Witness.Aux](blocks: Set[Set[F#Op]]): OpPartition[F] =
    fromPartition(Partition.fromSortedBlocks(blocks.map(_.map(_.index).to(SortedSet)).toSeq.sortBy(_.min)))

  implicit def partialOrder[F <: free.MonoidDef.Aux[F] with Singleton]: PartialOrder[OpPartition[F]] =
    PartialOrder[Partition].contramap(_.underlying)

  implicit def boundedLattice[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: BoundedLattice[OpPartition[F]] = new BoundedLattice[OpPartition[F]] {
    def zero: OpPartition[F] = new OpPartition[F](Partition.fromSortedBlocks(Seq.tabulate(valueOf[F].nOperators)(SortedSet(_))))
    def one: OpPartition[F] = new OpPartition[F](Partition.fromSortedBlocks(Seq((0 until valueOf[F].nOperators).to(SortedSet))))
    def meet(lhs: OpPartition[F], rhs: OpPartition[F]): OpPartition[F] = new OpPartition[F](MeetSemilattice[Partition].meet(lhs.underlying, rhs.underlying))
    def join(lhs: OpPartition[F], rhs: OpPartition[F]): OpPartition[F] = new OpPartition[F](JoinSemilattice[Partition].join(lhs.underlying, rhs.underlying))
  }

}
