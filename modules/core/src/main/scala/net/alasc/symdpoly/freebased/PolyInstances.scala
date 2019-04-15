package net.alasc.symdpoly.freebased

import scala.annotation.tailrec

import shapeless.Witness
import spire.algebra.{Action, Eq, Field, FieldAssociativeAlgebra, Involution}
import spire.syntax.cfor.cforRange

import cyclo.Cyclo

import net.alasc.symdpoly.free.{MutablePoly, MutableWord}
import net.alasc.symdpoly.math.Phase
import net.alasc.symdpoly.{free, valueOf}
import spire.syntax.involution._
import spire.syntax.eq._

final class PolyInstances[
  M <: MonoDef.Aux[F] with Singleton: Witness.Aux,
  F <: free.MonoDef.Aux[F] with Singleton
] extends FieldAssociativeAlgebra[Poly[M, F], Cyclo] with Involution[Poly[M, F]] with Eq[Poly[M, F]] with Action[Poly[M, F], M#PermutationType] {

  implicit def scalar: Field[Cyclo] = Cyclo.typeclasses

  def M: M = valueOf[M]
  implicit def witnessF: Witness.Aux[F] = M.witnessFree

  def adjoint(lhs: Poly[M, F]): Poly[M, F] = {
    val res = MutablePoly.empty[F](lhs.nTerms)
    cforRange(0 until lhs.nTerms) { i =>
      val newMono: MutableWord[F] = lhs.keys(i).mutableCopy()
      newMono.inPlaceAdjoint()
      M.inPlaceNormalForm(newMono: MutableWord[M#Free])
      val newCoeff = lhs.coeff(i).adjoint * newMono.phase.toCyclo
      res.add(newMono.setPhase(Phase.one).setImmutable(), newCoeff)
    }
    res.immutableCopy[M]
  }

  override def minus(lhs: Poly[M, F], rhs: Poly[M, F]): Poly[M, F] = {
    val res = MutablePoly.empty[F](lhs.nTerms + rhs.nTerms)
    cforRange(0 until lhs.nTerms) { i => res(lhs.monomialNormalForm(i)) = lhs.values(i) }
    cforRange(0 until rhs.nTerms) { j => res.add(rhs.monomialNormalForm(j), -rhs.coeff(j)) }
    res.immutableCopy[M]

  }

  def negate(lhs: Poly[M, F]): Poly[M, F] = {
    val newValues = lhs.values.map(x => -x)
    new Poly[M, F](lhs.keys, newValues)
  }

  def zero: Poly[M, F] = Poly.zero[M, F]

  def plus(lhs: Poly[M, F], rhs: Poly[M, F]): Poly[M, F] = {
    val res = MutablePoly.empty[F](lhs.nTerms + rhs.nTerms)
    cforRange(0 until lhs.nTerms) { i => res(lhs.monomialNormalForm(i)) = lhs.values(i) }
    cforRange(0 until rhs.nTerms) { j => res.add(rhs.monomialNormalForm(j), rhs.coeff(j)) }
    res.immutableCopy[M]
  }

  def one: Poly[M, F] = Poly.one[M, F]

  def times(lhs: Poly[M, F], rhs: Poly[M, F]): Poly[M, F] = {
    val res = MutablePoly.empty[F](lhs.nTerms * rhs.nTerms)
    cforRange(0 until lhs.nTerms) { i =>
      cforRange(0 until rhs.nTerms) { j =>
        val lm = lhs.monomialNormalForm(i)
        val rm = rhs.monomialNormalForm(j)
        val newMono = lm.mutableCopy(lm.length + rm.length)
        newMono *= rm
        valueOf[M].inPlaceNormalForm(newMono: MutableWord[M#Free])
        val newCoeff = lhs.coeff(i) * rhs.coeff(j) * newMono.phase.toCyclo
        newMono.setPhase(Phase.one)
        res.add(newMono.setImmutable(), newCoeff)
      }
    }
    res.immutableCopy[M]
  }

  def eqv(lhs: Poly[M, F], rhs: Poly[M, F]): Boolean = (lhs.nTerms == rhs.nTerms) && {
    @tailrec def rec(i: Int): Boolean =
      if (i == lhs.nTerms) true
      else if (lhs.coeff(i) =!= rhs.coeff(i)) false
      else if (lhs.monomialNormalForm(i) =!= rhs.monomialNormalForm(i)) false // we can compare MutableWords directly, as they have a sane equality test
      else rec(i + 1)
    rec(0)
  }

  def timesl(c: Cyclo, poly: Poly[M, F]): Poly[M, F] =
    if (c.isZero) zero else new Poly[M, F](poly.keys, poly.values.map(c * _))

  def actr(p: Poly[M, F], g: M#PermutationType): Poly[M, F] = {
    val res = free.MutablePoly.empty[F](p.nTerms)
    cforRange(0 until p.nTerms) { i =>
      val newMono = p.monomialNormalForm(i).mutableCopy()
      newMono.inPlaceGenPermAction(g.genPerm)
      M.inPlaceNormalForm(newMono)
      val newCoeff = p.coeff(i) * newMono.phase.toCyclo
      newMono.setPhase(Phase.one)
      res.add(newMono.setImmutable(), newCoeff)
    }
    res.immutableCopy[M]
  }

  def actl(g: M#PermutationType, p: Poly[M, F]): Poly[M, F] = actr(p, M.permutationGroup.inverse(g))

}
