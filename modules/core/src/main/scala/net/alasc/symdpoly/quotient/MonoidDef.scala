package net.alasc.symdpoly
package quotient

import net.alasc.algebra.PermutationAction
import net.alasc.symdpoly.free.{MutablePoly, MutableWord}
import spire.syntax.cfor._
import scala.annotation.tailrec

import net.alasc.bsgs.UnorderedPartitionStabilizer
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.symdpoly.math.{GenPerm, Phase}
import net.alasc.util._
import spire.syntax.eq._
import spire.syntax.group._
import cats.instances.vector._
import cats.instances.option._
import cats.instances.either._
import cats.syntax.traverse._
import cats.syntax.alternative._
import syntax.phased._
import net.alasc.symdpoly.util.{OrderedSet, SparseTrie}
import shapeless.Witness
import spire.util.Opt
import spire.syntax.action._
import net.alasc.util._
import net.alasc.symdpoly.freebased.{Mono, Poly}
import net.alasc.perms.default._

/** Tools for computation of symmetries of a quotient monoid. */
class Symmetries[
  M <: MonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] {

  def M: M = valueOf[M]

  def F: F = M.Free

  implicit def witnessF: Witness.Aux[F] = M.witnessFree

  val lhs = F.opIndexMap.elements.map(_.toMono) ++ M.rewritingRules.entries.map { case (key, _) => new Mono[F, F](key: MutableWord[F]) }
  val orderedSet = symmetries.Orbit.allElements(lhs, F.symmetryGroup.generators, F.monoPhased.phaseCanonical)
  val normalForms = orderedSet.toIndexedSeq.flatMap(m => (0 until M.cyclotomicOrder).map(num => m <* Phase(num, M.cyclotomicOrder))).map(M.quotient(_))
  val partition = Partition.fromSeq(normalForms)

  /** Permutation action of free permutations on the set of monomials described by orderedSet. */
  val action = new PermutationAction[freebased.Permutation[F, F]] {
    def isFaithful: Boolean = true
    def findMovedPoint(g: freebased.Permutation[F, F]): NNOption =
      g.genPerm.largestMovedPoint.mapInt( i => orderedSet.indexOf(F.opIndexMap.elements(i).toMono)*M.cyclotomicOrder)
    def movedPointsUpperBound(g: freebased.Permutation[F, F]): NNOption = NNSome(orderedSet.length * M.cyclotomicOrder - 1)
    def actl(g: freebased.Permutation[F, F], i: Int): Int = actr(i, g.inverse)
    def actr(i: Int, g: freebased.Permutation[F, F]): Int = {
      val preimage = orderedSet(i / M.cyclotomicOrder) <* Phase(i % M.cyclotomicOrder, M.cyclotomicOrder)
      val image = preimage <|+| g
      orderedSet.indexOf(image.phaseCanonical) * M.cyclotomicOrder + image.phaseOffset.numeratorIn(M.cyclotomicOrder)
    }
  }

  def isGroupCompatible(grp: Grp[F#PermutationType]): Boolean = grp.generators.forall(UnorderedPartitionStabilizer.partitionInvariantUnder(partition, action, _))

  def compatibleSubgroup(grp: Grp[F#PermutationType]): Grp[M#PermutationType] = {
    val subgroup: Grp[F#PermutationType] = if (isGroupCompatible(grp)) grp else grp.unorderedPartitionStabilizer(action, partition)
    M.groupInQuotientNC(subgroup)
  }

}

/** Base class for quotient monoids. */
abstract class MonoidDef extends freebased.MonoidDef {
  monoidDef =>

  def cyclotomicOrder: Int = Free.cyclotomicOrder

  def rewritingRules: SparseTrie[MutableWord[Free], MutableWord[Free]]

  def maximalLhsLength: Int

  private[this] lazy val symmetries: Symmetries[this.type, Free] = new Symmetries[this.type, Free]

  /** Returns the subgroup of a group of permutations on the free variables, such that it is the maximal subgroup
    * compatible with the quotient structure. */
  def groupInQuotient(grp: Grp[freebased.Permutation[Free, Free]]): Grp[PermutationType] = {
    val s = symmetries
    s.compatibleSubgroup(grp)
  }

  /** Translates a group acting on the free variables into a group acting on the equivalence classes on the quotient
    * monoid, assuming that the group is compatible without verification. */
  def groupInQuotientNC(grp: Grp[freebased.Permutation[Free, Free]]): Grp[PermutationType] =
    Grp.fromGeneratorsAndOrder(grp.generators.map(quotientNC), grp.order)

  /** Symmetry group that preserves the quotient structure, with elements that act by permuting operator variables,
    * possibly applying a phase. */
  lazy val symmetryGroup: Grp[PermutationType] = groupInQuotient(Free.symmetryGroup)

  /** Returns the permutation of the quotient monoid equivalence classes that correspond to the free permutation given,
    * without performing sanity checks. */
  def quotientNC(permutation: Free#PermutationType): freebased.Permutation[monoidDef.type, Free] =
    new freebased.Permutation[monoidDef.type, Free](permutation.genPerm)

  /** Returns the permutation of the quotient monoid equivalence classes that correspond to the given free permutation
    * when it is compatible, or returns None otherwise.
    */
  def quotient(permutation: Free#PermutationType): Option[freebased.Permutation[monoidDef.type, Free]] = {
    import symmetries.{partition, action}
    if (UnorderedPartitionStabilizer.partitionInvariantUnder(partition, action, permutation)) Some(quotientNC(permutation)) else None
  }

  /** Returns the representative class of the given free monomial. */
  def quotient(word: Mono[Free, Free]): MonoType = {
    val res = word.data.mutableCopy()
    inPlaceNormalForm(res)
    new Mono[monoidDef.type, Free](res.setImmutable())
  }

  /** Returns the representative class of the given free polynomial. */
  def quotient(poly: Poly[Free, Free]): Poly[monoidDef.type, Free] =
    if (poly.nTerms == 0) freebased.Poly.zero[monoidDef.type, Free] else {
      val res = MutablePoly.empty[Free](poly.nTerms)
      cforRange(0 until poly.nTerms) { i =>
        val newMono = poly.monomialNormalForm(i).mutableCopy()
        inPlaceNormalForm(newMono)
        val newCoeff = poly.coeff(i) * newMono.phase.toCyclo
        newMono.setPhase(Phase.one)
        res.add(newMono.setImmutable(), newCoeff)
      }
      res.immutableCopy[monoidDef.type]
    }

  /** Rewrites the given word in the free variables to its normal form in the quotient monoid.
    * Returns whether any rule application has taken place (even if the rule applications left the word
    * invariant in the end).
    */
  def inPlaceNormalForm(word: MutableWord[Free], start: Int = 0): Boolean =
    if (word.isZero) false else {
      @tailrec def rec(i: Int, modified: Boolean): Boolean =
        if (i < 0)
          rec(0, modified)
        else if (i >= word.length)
          modified
        else {
          /** Tries to find a match from indices i to j and returns whether a rewriting occurred. */
          @tailrec def findMatch(j: Int, branch: SparseTrie[MutableWord[Free], MutableWord[Free]]): Boolean =
            if (j == word.length) false else branch.child(word.indices(j)) match {
              case SparseTrie.IsBranch(newBranch) => findMatch(j + 1, newBranch)
              case SparseTrie.IsLeaf(value) =>
                word.replaceRange(i, j + 1, value)
                true
              case _ => false
            }
          if (findMatch(i, rewritingRules))
            rec(i - maximalLhsLength + 1, true)
          else
            rec(i + 1, modified)
        }
      rec(0, false)
    }

}

object MonoidDef {

  type Aux[F <: free.MonoidDef with Singleton] = quotient.MonoidDef { type Free = F }

}
