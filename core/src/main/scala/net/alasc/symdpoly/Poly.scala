package net.alasc.symdpoly

import cats.evidence.Is

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3
import shapeless.Witness
import spire.algebra.{Action, Eq, FieldAssociativeAlgebra, Involution}
import spire.math.{Rational, Searching}
import spire.syntax.cfor._
import spire.syntax.involution._
import spire.syntax.order._
import cyclo.Cyclo
import metal.mutable.{HashMap => MMap}
import metal.syntax._
import net.alasc.symdpoly.free.{MutablePoly, MutableWord}
import net.alasc.symdpoly.generic.{FreeBasedMonoidDef, MonoidDef}
import net.alasc.symdpoly.math.GenPerm
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Predicate
import algebra.Phased.syntax._

trait PolyTerm[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton] { lhs =>

  // Abstract method to be implemented
  def toPoly: Poly[M, F]
  def +(rhs: Poly[M, F]): Poly[M, F]
  def *(rhs: Poly[M, F]): Poly[M, F]

  // +/- scalar
  def +(rhs: Int)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + (rhs: Cyclo)
  def +(rhs: Rational)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + (rhs: Cyclo)
  def +(rhs: Phase)(implicit d: DummyImplicit, wM: Witness.Aux[M]): Poly[M, F] = lhs + (rhs: Cyclo)
  def +(rhs: Cyclo)(implicit wM: Witness.Aux[M]): Poly[M, F] =
    if (rhs.isZero) toPoly else lhs + Poly.constant[M, F](rhs)

  def -(rhs: Int)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + (-rhs)
  def -(rhs: Rational)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + (-rhs)
  def -(rhs: Phase)(implicit d: DummyImplicit, wM: Witness.Aux[M]): Poly[M, F] = lhs + (-rhs)
  def -(rhs: Cyclo)(implicit wM: Witness.Aux[M]): Poly[M, F] =
    if (rhs.isZero) toPoly else lhs + (-rhs)

  // +/- op

  def +(rhs: F#Op)(implicit wM: Witness.Aux[M], ev: Mono[F, F] Is Mono[M, F]): Poly[M, F] = lhs + ev.coerce(rhs.toMono)
  def -(rhs: F#Op)(implicit wM: Witness.Aux[M], ev: Mono[F, F] Is Mono[M, F]): Poly[M, F] = lhs - ev.coerce(rhs.toMono)

  // +/- phasedOp
  def +(rhs: F#PhasedOp)(implicit wM: Witness.Aux[M], ev: Mono[F, F] Is Mono[M, F]): Poly[M, F] = lhs + ev.coerce(rhs.toMono)
  def -(rhs: F#PhasedOp)(implicit wM: Witness.Aux[M], ev: Mono[F, F] Is Mono[M, F]): Poly[M, F] = lhs - ev.coerce(rhs.toMono)

  // +/- mono

  def +(rhs: Mono[M, F])(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + rhs.toPoly
  def -(rhs: Mono[M, F])(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs + (-rhs).toPoly

  // * / scalar

  def *(rhs: Int)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * (rhs: Cyclo)
  def *(rhs: Rational)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * (rhs: Cyclo)
  def *(rhs: Cyclo)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * Poly.constant[M, F](rhs)

  def /(rhs: Int)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * Cyclo(rhs).reciprocal
  def /(rhs: Rational)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * rhs.reciprocal
  def /(rhs: Cyclo)(implicit wM: Witness.Aux[M]): Poly[M, F] = lhs * rhs.reciprocal
}

abstract class GenPoly[M <: MonoidDef with Singleton] {
  def nTerms: Int
  def monomial(i: Int): M#Monomial
  def coeff(i: Int): Cyclo
  def string(leftBracket: String = "", rightBracket: String = ""): String
}

class Poly[
  M <: FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](private[this] val keys: Array[MutableWord[F]],
  private[this] val values: Array[Cyclo])
 (implicit val wM: Witness.Aux[M]) extends GenPoly[M] with PolyTerm[M, F] { lhs =>

  def toPoly: Poly[M, F] = lhs

  def M: M = wM.value
  def F: F = M.Free
  implicit def wF: Witness.Aux[F] = F.witness

  def eqv(rhs: Poly[M, F]): Boolean = (lhs.nTerms == rhs.nTerms) && {
    @tailrec def rec(i: Int): Boolean =
      if (i == lhs.nTerms) true
      else if (lhs.coeff(i) =!= rhs.coeff(i)) false
      else if (lhs.monomialNormalForm(i) =!= rhs.monomialNormalForm(i)) false // we can compare MutableWords directly, as they have a sane equality test
      else rec(i + 1)
    rec(0)
  }

  def normalForm: Poly[F, F] = new Poly[F, F](keys, values)

  def nTerms: Int = keys.length
  def monomialNormalForm(i: Int): MutableWord[F] = keys(i)
  def monomial(i: Int): Mono[M, F] = new Mono[M ,F](keys(i))
  def coeff(i: Int): Cyclo = values(i)
  def coeff(mono: Mono[M, F]): Cyclo = {
    require(!mono.isZero)
    require(mono.phaseOffset == Phase.one)
    val i = Searching.search(keys, mono.data)
    if (i >= 0) coeff(i) else Cyclo.zero
  }

  /** Degree of the polynomial if it is nonzero, or -1 if it is zero. */
  def degree(implicit ev: M =:= F): Int = {
    @tailrec def findMax(i: Int, d: Int): Int =
      if (i == nTerms) d else findMax(i + 1, spire.math.max(d, keys(i).length))
    findMax(0, -1)
  }

  def mutableCopy(implicit ev: M =:= F): MutablePoly[F] = {
    val terms = MMap.empty[MutableWord[F], Cyclo]
    cforRange(0 until nTerms) { i => terms(keys(i)) = values(i) }
    new MutablePoly[F](terms)
  }

  // Object methods

  def string(leftBracket: String = "", rightBracket: String = ""): String =
    if (nTerms == 0) "0" else {
      def combine(s: String, m: String): String =
        if (m == "") s
        else if (s == "1") m
        else s + " " + m
      def first(s: String, m: String): String =
        if (s.startsWith("-")) "- " + combine(s.tail, m) else combine(s, m)
      def middle(s: String, m: String): String =
        if (s.startsWith("-")) " - " + combine(s.tail, m) else " + " + combine(s, m)
      @tailrec def rec(i: Int, acc: String): String =
        if (i == nTerms) acc else rec(i + 1, acc + middle(coeff(i).toString, leftBracket + monomial(i).toString + rightBracket))
      rec(1, first(coeff(0).toString, leftBracket + monomial(0).toString + rightBracket))
    }

  override def toString: String = string()

  override def equals(any: Any): Boolean = any match {
    case rhs: Poly[M, F] if (lhs.M eq rhs.M) => lhs.eqv(rhs)
    case _ => false
  }

  lazy val hash: Int = {
    var h = MurmurHash3.arraySeed
    cforRange(0 until nTerms) { i =>
      h = MurmurHash3.mix(h, keys(i).hashCode) // MutableWord has a sensible hashCode implementation
      h = MurmurHash3.mix(h, values(i).hashCode)
    }
    MurmurHash3.finalizeHash(h, nTerms)
  }

  override def hashCode: Int = hash

  def adjoint: Poly[M, F] = {
    val res = MutablePoly.empty[F](lhs.nTerms)
    cforRange(0 until nTerms) { i =>
      val newMono: MutableWord[F] = lhs.keys(i).mutableCopy()
      newMono.inPlaceAdjoint()
      M.inPlaceNormalForm(newMono: MutableWord[M#Free])
      val newCoeff = lhs.coeff(i).adjoint * newMono.phase.toCyclo
      res.add(newMono.setPhase(Phase.one).setImmutable(), newCoeff)
    }
    res.immutableCopy[M]
  }

  def unary_- : Poly[M, F] = {
    val newValues = values.map(x => -x)
    new Poly[M, F](keys, newValues)
  }

  def *(rhs: Poly[M, F]): Poly[M, F] = {
    val res = MutablePoly.empty[F](lhs.nTerms * rhs.nTerms)
    cforRange(0 until lhs.nTerms) { i =>
      cforRange(0 until rhs.nTerms) { j =>
        val lm = lhs.monomialNormalForm(i)
        val rm = rhs.monomialNormalForm(j)
        val newMono = lm.mutableCopy(lm.length + rm.length)
        newMono *= rm
        M.inPlaceNormalForm(newMono: MutableWord[M#Free])
        val newCoeff = lhs.coeff(i) * rhs.coeff(j) * newMono.phase.toCyclo
        newMono.setPhase(Phase.one)
        res.add(newMono.setImmutable(), newCoeff)
      }
    }
    res.immutableCopy[M]
  }

  def *(rhs: Mono[M, F]): Poly[M, F] = {
    val res = MutablePoly.empty[F](lhs.nTerms)
    val rm = rhs.data
    cforRange(0 until lhs.nTerms) { i =>
      val lm = lhs.keys(i)
      val newMono = lm.mutableCopy(lm.length + rm.length)
      newMono *= rm
      M.inPlaceNormalForm(newMono: MutableWord[M#Free])
      val newCoeff = lhs.coeff(i) * newMono.phase.toCyclo
      newMono.setPhase(Phase.one)
      res.add(newMono.setImmutable(), newCoeff)
    }
    res.immutableCopy[M]
  }

  def *(rhs: Int): Poly[M, F] = lhs * Cyclo(rhs)
  def *(rhs: Rational): Poly[M, F] = lhs * Cyclo(rhs)
  def *(rhs: Cyclo): Poly[M, F] =
    if (rhs.isZero) Poly.zero[M, F] else new Poly[M, F](keys, values.map(_ * rhs))

  def /(rhs: Int): Poly[M, F] = lhs / Cyclo(rhs)
  def /(rhs: Rational): Poly[M, F] = lhs / Cyclo(rhs)
  def /(rhs: Cyclo): Poly[M, F] = lhs * (rhs.reciprocal)

  def *:(realLhs: Cyclo): Poly[M, F] =
    if (realLhs.isZero) Poly.zero[M, F] else new Poly[M, F](keys, values.map(realLhs * _))

  def +(rhs: Poly[M, F]): Poly[M, F] = {
    val res = MutablePoly.empty[F](lhs.nTerms + rhs.nTerms)
    cforRange(0 until lhs.nTerms) { i => res(monomialNormalForm(i)) = values(i) }
    cforRange(0 until rhs.nTerms) { j => res.add(rhs.monomialNormalForm(j), rhs.coeff(j)) }
    res.immutableCopy[M]
  }

  def -(rhs: Poly[M, F]): Poly[M, F] = {
    val res = MutablePoly.empty[F](lhs.nTerms + rhs.nTerms)
    cforRange(0 until lhs.nTerms) { i => res(monomialNormalForm(i)) = values(i) }
    cforRange(0 until rhs.nTerms) { j => res.add(rhs.monomialNormalForm(j), -rhs.coeff(j)) }
    res.immutableCopy[M]
  }
}

object Poly {

  implicit def polyTermToPoly[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](polyTerm: PolyTerm[M, F]): Poly[M, F] =
    polyTerm.toPoly

  def zero[M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux, F <: free.MonoidDef.Aux[F] with Singleton]: Poly[M, F] =
    new Poly[M, F](new Array[MutableWord[F]](0), new Array[Cyclo](0))

  def one[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit wM: Witness.Aux[M]): Poly[M, F] = {
    implicit def wF: Witness.Aux[F] = (wM.value: M).witnessFree
    new Poly[M, F](Array(MutableWord.one[F].setImmutable()), Array(Cyclo.one))
  }

  def constant[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](int: Int)(implicit wM: Witness.Aux[M]): Poly[M, F] = constant(Cyclo(int))

  def constant[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](phase: Phase)(implicit d: DummyImplicit, wM: Witness.Aux[M]): Poly[M, F] = constant(phase.toCyclo)

  def constant[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](rational: Rational)(implicit wM: Witness.Aux[M]): Poly[M, F] = constant(Cyclo(rational))

  def constant[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](cyclo: Cyclo)(implicit wM: Witness.Aux[M]): Poly[M, F] = {
    implicit def wF: Witness.Aux[F] = (wM.value: M).witnessFree
    if (cyclo.isZero) zero[M, F] else new Poly[M, F](Array(MutableWord.one[F]), Array(cyclo))
  }

  def single[M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux, F <: free.MonoidDef.Aux[F] with Singleton](mono: Mono[M, F], coeff: Cyclo): Poly[M, F] =
    if (coeff.isZero || mono.isZero) zero[M, F]
    else if (mono.data.phase.isOne) new Poly[M, F](Array(mono.data), Array(coeff))
    else {
      val res = mono.data.mutableCopy()
      val newCoeff = coeff * res.phase.toCyclo
      new Poly[M, F](Array(res.setPhase(Phase.one).setImmutable()), Array(newCoeff))
    }

  def apply[M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux, F <: free.MonoidDef.Aux[F] with Singleton](mono: Mono[M, F]): Poly[M, F] =
    if (mono.isZero) zero[M, F]
    else if (mono.data.phase.isOne) new Poly[M, F](Array(mono.data), Array(Cyclo.one))
    else {
      val newMono = mono.data.mutableCopy()
      val phase = newMono.phase
      newMono.setPhase(Phase.one)
      new Poly[M, F](Array(newMono.setImmutable()), Array(phase.toCyclo))
    }

  def apply[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](terms: (Mono[M, F], Cyclo)*)(implicit wM: Witness.Aux[M]): Poly[M, F] = {
    implicit def wF: Witness.Aux[F] = (wM.value: M).witnessFree
    if (terms.size == 0) zero[M, F]
    else if (terms.size == 1) single[M, F](terms(0)._1, terms(0)._2)
    else {
      val res = MutablePoly.empty[F](terms.size)
      cforRange(0 until terms.size) { i =>
        val mono = terms(i)._1
        val coeff = terms(i)._2
        if (mono.data.phase.isOne)
          res.add(mono.data, coeff)
        else {
          val newMono = mono.data.mutableCopy()
          val newCoeff = coeff * newMono.phase.toCyclo
          newMono.setPhase(Phase.one)
          res.add(newMono.setImmutable(), newCoeff)
        }
      }
      res.immutableCopy
    }
  }

  // Typeclasses

  implicit def predicate[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton]: Predicate[Poly[M, F]] = {
    poly => poly.nTerms > 0
  }
  implicit def associativeAlgebra[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit wM: Witness.Aux[M]): FieldAssociativeAlgebra[Poly[M, F], Cyclo] =
    (wM.value: M).polyAssociativeAlgebra
  implicit def involution[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit wM: Witness.Aux[M]): Involution[Poly[M, F]] =
    (wM.value: M).polyInvolution
  implicit def equ[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit wM: Witness.Aux[M]): Eq[Poly[M, F]] =
    (wM.value: M).polyEq
  implicit def genPermAction[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit wM: Witness.Aux[M]): Action[Poly[M, F], GenPerm] =
    (wM.value: M).polyGenPermAction

}


final class PolyGenPermAction[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit wM: Witness.Aux[M])
  extends Action[Poly[M, F], GenPerm] {
  def M: M = wM.value
  implicit def wF: Witness.Aux[F] = M.witnessFree
  def actr(p: Poly[M, F], g: GenPerm): Poly[M, F] = {
    val res = free.MutablePoly.empty[F](p.nTerms)
    cforRange(0 until p.nTerms) { i =>
      val newMono = p.monomialNormalForm(i).mutableCopy()
      newMono.applyGenPermAction(g)
      M.inPlaceNormalForm(newMono)
      val newCoeff = p.coeff(i) * newMono.phase.toCyclo
      newMono.setPhase(Phase.one)
      res.add(newMono.setImmutable(), newCoeff)
    }
    res.immutableCopy[M]
  }
  def actl(g: GenPerm, p: Poly[M, F]): Poly[M, F] = actr(p, g.inverse)
}
