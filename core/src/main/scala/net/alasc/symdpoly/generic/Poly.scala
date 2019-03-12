package net.alasc.symdpoly
package generic

import shapeless.Witness
import spire.math.Rational
import spire.syntax.action._

import syntax.phased._
import cyclo.Cyclo
import spire.syntax.cfor._
import spire.syntax.group._
import spire.syntax.ring._
import net.alasc.util._
import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.symdpoly.math.Phase
import net.alasc.symdpoly.util.OrderedSet
import syntax.phased._
import net.alasc.perms.default._

abstract class Poly[M <: generic.MonoidDef with Singleton:Witness.Aux] { lhs: M#Polynomial =>

  def M: M = valueOf[M]

  def nTerms: Int
  def monomial(i: Int): M#Monomial
  def coeff(i: Int): Cyclo
  def coeff(mono: M#Monomial): Cyclo
  def string(leftBracket: String = "", rightBracket: String = ""): String

  def *(rhs: Int): M#Polynomial
  def *(rhs: Rational): M#Polynomial
  def *(rhs: Cyclo): M#Polynomial

  def /(rhs: Int): M#Polynomial
  def /(rhs: Rational): M#Polynomial
  def /(rhs: Cyclo): M#Polynomial

  def allMonomialsUnderOrbit(grp: Grp[M#Permutation]): OrderedSet[M#Monomial] = {
    val monomials: Iterator[M#Monomial] = grp.iterator.flatMap(g => Iterator.tabulate(lhs.nTerms)(i => (lhs.monomial(i) <|+| g).phaseCanonical))
    val array = monomials.toArray
    spire.math.Sorting.quickSort(array)(valueOf[M].monoOrder, implicitly)
    new OrderedSet(array.map(_.asInstanceOf[AnyRef]))
  }

  def invariantSubgroupOf(grp: Grp[M#Permutation]): Grp[M#Permutation] = {
    val monomials: OrderedSet[M#Monomial] = allMonomialsUnderOrbit(grp)
    val order = M.cyclotomicOrder
    val monomialsAction: PermutationAction[M#Permutation] = new PermutationAction[M#Permutation] {
      def isFaithful: Boolean = false
      def findMovedPoint(g: M#Permutation): NNOption = {
        cforRange(0 until monomials.length * order) { i =>
          if (actr(i, g) != i) return NNSome(i)
        }
        NNNone
      }
      def movedPointsUpperBound(g: M#Permutation): NNOption = NNSome(monomials.length * order - 1)
      def actr(p: Int, g: M#Permutation): Int = {
        val phase = Phase(p % order, order)
        val index = p / order
        val res = M.permutationMonoAction.actr(monomials(index), g)
        val canonical = res.phaseCanonical
        val newPhase = res.phaseOffset * phase
        val newIndex = monomials.indexOf(canonical)
        newIndex * order + newPhase.numeratorIn(order)
      }
      def actl(g: M#Permutation, p: Int): Int = actr(p, g.inverse)
    }
    val nMonomials = monomials.length
    val coeffSeq = for {
      mono <- monomials.iterator.toVector
      coeff = lhs.coeff(mono: M#Monomial)
      k <- 0 until order
      phase = Phase(k, order)
    } yield coeff * phase.toCyclo
    val partition = Partition.fromSeq(coeffSeq)
    val stabilizer = grp.orderedPartitionStabilizer(monomialsAction, partition)
    val generators = stabilizer.smallGeneratingSet
    Grp.fromGeneratorsAndOrder(generators, stabilizer.order)
  }

}
