package net.alasc.symdpoly
package generic

import cats.kernel.Eq
import shapeless.Witness
import spire.algebra.{Action, Involution, MultiplicativeMonoid, Order}

import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import net.alasc.symdpoly.free.MutableWord
import net.alasc.symdpoly.math.GenPerm
import net.alasc.symdpoly.{Phase, Poly, PolyTerm, free, valueOf}
import org.typelevel.discipline.Predicate

/** An element of a [[FreeBasedMonoidDef]], which represents a monomial in a polynomial ring.
  *
  * @param data Normal form of the monoid element, along with a possible phase
  */
class FreeBasedMono[
  M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](protected[symdpoly] val data: MutableWord[F]) extends PolyTerm[M, F] { lhs =>

  require(!data.mutable)
  require(F.cyclotomicOrder % data.phase.n == 0)

  def M: M = valueOf[M]
  def F: F = (M: M).Free
  implicit def witnessF: Witness.Aux[F] = (F: F).witness

  // Java based methods

  override def toString: String = if (M eq F) data.toString else s"[$data]"
  override def equals(any: Any): Boolean = any match {
    case rhs: FreeBasedMono[M, F] if (lhs.M eq rhs.M) && (lhs.F eq rhs.F) => lhs.data == rhs.data
    case _ => false
  }
  override def hashCode: Int = data.hashCode

  //

  def normalForm: FreeBasedMono[F, F] = new FreeBasedMono[F, F](data)
  def isZero(implicit mb: MultiplicativeBinoid[FreeBasedMono[M, F]], equ: Eq[FreeBasedMono[M, F]]): Boolean = mb.isZero(lhs)
  def isOne(implicit mm: MultiplicativeMonoid[FreeBasedMono[M, F]], equ: Eq[FreeBasedMono[M, F]]): Boolean = mm.isOne(lhs)
  def unary_- (implicit phased: Phased[FreeBasedMono[M, F]]): FreeBasedMono[M, F] = phased.gtimesl(Phase.minusOne, lhs)
  def *(rhs: Phase)(implicit phased: Phased[FreeBasedMono[M, F]]): FreeBasedMono[M, F] = phased.gtimesr(lhs, rhs)
  def *(rhs: FreeBasedMono[M, F])(implicit mm: MultiplicativeMonoid[FreeBasedMono[M, F]]): FreeBasedMono[M, F] = mm.times(lhs, rhs)
  def adjoint(implicit inv: Involution[FreeBasedMono[M, F]]): FreeBasedMono[M, F] = inv.adjoint(lhs)
  def pow(rhs: Int)(implicit mm: MultiplicativeMonoid[FreeBasedMono[M, F]]): FreeBasedMono[M, F] = mm.pow(lhs, rhs)

  // methods that are valid when F =:= M
  def length(implicit ev: F =:= M): Int = data.length
  def apply(i: Int)(implicit ev: F =:= M): F#Op = data(i)
  def phase(implicit ev: F =:= M): Phase = data.phase
  def mutableCopy(implicit ev: F =:= M): MutableWord[F] = data.mutableCopy

  // conversion to polynomials
  def toPoly: Poly[M, F] = Poly(lhs)
  def +(rhs: Poly[M, F]): Poly[M, F] = lhs.toPoly + rhs
  def *(rhs: Poly[M, F]): Poly[M, F] = lhs.toPoly * rhs

}

object FreeBasedMono {

  implicit def monoTermToMono[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](monoTerm: FreeBasedMonoTerm[M, F]): FreeBasedMono[M, F] =
    monoTerm.toMono


  type Free[F <: free.MonoidDef.Aux[F] with Singleton] = FreeBasedMono[F, F]

  // Factory methods that work for any monoid, free or quotient

  /** Zero monoid element. */
  def zero[M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: FreeBasedMono[M, F] =
    new FreeBasedMono[M, F](MutableWord.zero[F].setImmutable())

  /** Empty word, i.e. one / the identity. */
  def one[M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: FreeBasedMono[M, F] =
    new FreeBasedMono[M, F](MutableWord.one[F].setImmutable())

  // all the overloads of apply to workaround the issue shapeless#843

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](): FreeBasedMono[F, F] = one[F, F]

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase): FreeBasedMono[F, F] =
    new FreeBasedMono[F, F](MutableWord[F](phase).setImmutable())

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phasedOp: F#PhasedOp): FreeBasedMono[F, F] =
    new FreeBasedMono[F, F](MutableWord[F](phasedOp.phase, Seq(phasedOp.op)).setImmutable())

  // apply(op, op, ...)
  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](op1: F#Op): FreeBasedMono[F, F] =
    fromSeq(Seq(op1))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](op1: F#Op, op2: F#Op): FreeBasedMono[F, F] =
    fromSeq(Seq(op1, op2))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](op1: F#Op, op2: F#Op, op3: F#Op): FreeBasedMono[F, F] =
    fromSeq(Seq(op1, op2, op3))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](op1: F#Op, op2: F#Op, op3: F#Op, op4: F#Op): FreeBasedMono[F, F] =
    fromSeq(Seq(op1, op2, op3, op4))

  // apply(phase, op, op, ...)

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase, op1: F#Op): FreeBasedMono[F, F] =
    fromSeq(phase, Seq(op1))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase, op1: F#Op, op2: F#Op): FreeBasedMono[F, F] =
    fromSeq(phase, Seq(op1, op2))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase, op1: F#Op, op2: F#Op, op3: F#Op): FreeBasedMono[F, F] =
    fromSeq(phase, Seq(op1, op2, op3))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase, op1: F#Op, op2: F#Op, op3: F#Op, op4: F#Op): FreeBasedMono[F, F] =
    fromSeq(phase, Seq(op1, op2, op3, op4))

  def fromSeq[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](ops: Seq[F#Op]): FreeBasedMono[F, F] =
    new FreeBasedMono[F, F](MutableWord(ops).setImmutable())

  def fromSeq[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase, ops: Seq[F#Op]): FreeBasedMono[F, F] =
    new FreeBasedMono[F, F](MutableWord(phase, ops).setImmutable())

  implicit def fromOp[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](op: F#Op): FreeBasedMono[F, F] = apply(op)

  // Typeclasses

  implicit def predicate[M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton]: Predicate[FreeBasedMono[M, F]] = Predicate(gw => !gw.data.isZero)

  implicit def involution[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]): Involution[FreeBasedMono[M, F]] = (wM.value: M).monoInvolution

  implicit def multiplicativeBinoid[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]): MultiplicativeBinoid[FreeBasedMono[M, F]] = (wM.value: M).monoMultiplicativeBinoid

  implicit def order[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]): Order[FreeBasedMono[M, F]] = (wM.value: M).monoOrder

  implicit def genPermAction[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]): Action[FreeBasedMono[M, F], GenPerm] = (wM.value: M).monoGenPermAction

  implicit def phased[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]): Phased[FreeBasedMono[M, F]] = (wM.value: M).monoPhased

  final class MonoGenPermAction[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]) extends Action[FreeBasedMono[M, F], GenPerm] {
    def M: M = wM.value
    def actr(m: FreeBasedMono[M, F], g: GenPerm): FreeBasedMono[M, F] = {
      val res = m.data.mutableCopy()
      res.inPlaceGenPermAction(g)
      M.inPlaceNormalForm(res)
      new FreeBasedMono[M, F](res.setImmutable())
    }
    def actl(g: GenPerm, m: FreeBasedMono[M, F]): FreeBasedMono[M, F] = actr(m, g.inverse)
  }

  final class MonoInstances[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]) extends MultiplicativeBinoid[FreeBasedMono[M, F]]
    with Involution[FreeBasedMono[M, F]]
    with Order[FreeBasedMono[M, F]] {
    implicit def wF: Witness.Aux[F] = (wM.value: M).witnessFree
    def M: M = wM.value
    def F: F = wF.value
    def zero: FreeBasedMono[M, F] = FreeBasedMono.zero[M, F]
    override def isZero(a: FreeBasedMono[M, F])(implicit ev: Eq[FreeBasedMono[M, F]]): Boolean = a.data.isZero
    def one: FreeBasedMono[M, F] = FreeBasedMono.one[M, F]
    override def isOne(a: FreeBasedMono[M, F])(implicit ev: Eq[FreeBasedMono[M, F]]): Boolean = a.data.isOne
    def adjoint(lhs: FreeBasedMono[M, F]): FreeBasedMono[M, F] =
      if (lhs.isZero || lhs.isOne) lhs else {
        val res = lhs.data.mutableCopy()
        res.inPlaceAdjoint()
        lhs.M.inPlaceNormalForm(res)
        new FreeBasedMono[M, F](res.setImmutable())
      }

    def compare(x: FreeBasedMono[M, F], y: FreeBasedMono[M, F]): Int = x.data.compareTo(y.data)
    def times(lhs: FreeBasedMono[M, F], rhs: FreeBasedMono[M, F]): FreeBasedMono[M, F] =
      if (lhs.isZero) lhs
      else if (lhs.isOne) rhs
      else if (rhs.isZero) rhs
      else if (rhs.isOne) lhs
      else {
        val res = lhs.data.mutableCopy(lhs.data.length + rhs.data.length)
        res *= rhs.data
        M.inPlaceNormalForm(res) // TODO: update
        new FreeBasedMono[M, F](res.setImmutable())
      }

  }

  final class MonoPhased[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]) extends Phased[FreeBasedMono[M, F]] {
    def phaseOffset(mono: FreeBasedMono[M, F]): Phase = mono.data.phase
    def phaseCanonical(mono: FreeBasedMono[M, F]): FreeBasedMono[M, F] = new FreeBasedMono[M, F](mono.data.mutableCopy().setPhase(Phase.one).setImmutable())
    def gtimesl(phase: Phase, mono: FreeBasedMono[M, F]): FreeBasedMono[M, F] = gtimesr(mono, phase.reciprocal)
    def gtimesr(mono: FreeBasedMono[M, F], phase: Phase): FreeBasedMono[M, F] = new FreeBasedMono[M, F]((mono.data.mutableCopy() *= phase).setImmutable())
  }

}