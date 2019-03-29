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

abstract class Poly[M <: generic.MonoidDef with Singleton:Witness.Aux] { lhs: M#PolyType =>

  def M: M = valueOf[M]

  def isZero: Boolean

  def degree: Int = Iterator.tabulate(nTerms)(monomial(_).degree).max
  def nTerms: Int
  def monomial(i: Int): M#MonoType
  def coeff(i: Int): Cyclo
  def coeff(mono: M#MonoType): Cyclo
  def string(leftBracket: String = "", rightBracket: String = ""): String

  def *(rhs: Int): M#PolyType
  def *(rhs: Rational): M#PolyType
  def *(rhs: Cyclo): M#PolyType

  def /(rhs: Int): M#PolyType
  def /(rhs: Rational): M#PolyType
  def /(rhs: Cyclo): M#PolyType

  def invariantSubgroupOf(grp: Grp[M#PermutationType]): Grp[M#PermutationType] =
    symmetries.invariantSubgroupOf((0 until nTerms).map(monomial), (x: M#MonoType) => coeff(x), grp, M.cyclotomicOrder)

}
