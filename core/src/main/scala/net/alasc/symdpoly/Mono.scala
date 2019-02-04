package net.alasc.symdpoly

import cats.evidence.Is
import shapeless.Witness
import spire.algebra.{Action, Involution, Order}
import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import net.alasc.symdpoly.free._
import net.alasc.symdpoly.generic.{MonoidDef => _, _}
import net.alasc.symdpoly.math.GenPerm
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Predicate

trait MonoTerm[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton] {
  lhs =>

  // Abstract methods to overload
  def toMono: Mono[M, F]
  def *(rhs: Mono[M, F]): Mono[M, F]

  def pow(rhs: Int): Mono[M, F] = toMono.pow(rhs)

  def *(rhs: F#Op)(implicit wM: Witness.Aux[M], ev: Mono[F, F] Is Mono[M, F]): Mono[M, F] = lhs * ev.coerce(rhs.toMono)

  def *(rhs: F#PhasedOp)(implicit wM: Witness.Aux[M], ev: Mono[F, F] Is Mono[M, F]): Mono[M, F] = lhs * (ev.coerce(rhs.toMono): Mono[M, F])
}

/** An element of a MonoidDef, which represents a monomial in a polynomial ring.
  *
  * @param data Normal form of the monoid element, with a possible phase
  */
class Mono[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](protected[symdpoly] val data: MutableWord[F])(implicit wM: Witness.Aux[M]) extends PolyTerm[M, F] { lhs =>
  require(!data.mutable)
  implicit def wF: Witness.Aux[F] = data.wF
  def M: M = wM.value

  def F: F = wF.value
  override def toString: String = if (M eq F) data.toString else s"[$data]"
  override def equals(any: Any): Boolean = any match {
    case rhs: Mono[M, F] if (lhs.M eq rhs.M) && (lhs.F eq rhs.F) => lhs.data == rhs.data
    case _ => false
  }
  override def hashCode: Int = data.hashCode
  def normalForm: Mono[F, F] = new Mono[F, F](data)
  def compareTo(rhs: Mono[M, F]): Int = lhs.data.compareTo(rhs.data)

  def isZero: Boolean = data.isZero
  def isOne: Boolean = data.isOne
  def unary_- : Mono[M, F] =
    new Mono[M, F](data.mutableCopy().multiplyBySignOf(-1).setImmutable())
  def timesSignOf(rhs: Int): Mono[M, F] =
    if (rhs == 0) Mono.zero[M, F]
    else if (rhs > 0) this
    else new Mono[M, F](data.mutableCopy().multiplyBySignOf(-1).setImmutable())
  def *(rhs: Phase): Mono[M, F] =
    new Mono[M, F]((data.mutableCopy() *= rhs).setImmutable())
  def *(rhs: Mono[M, F]): Mono[M, F] =
    if (lhs.isZero) lhs
    else if (lhs.isOne) rhs
    else if (rhs.isZero) rhs
    else if (rhs.isOne) lhs
    else {
      val res = lhs.data.mutableCopy(lhs.data.length + rhs.data.length)
      res *= rhs.data
      M.inPlaceNormalForm(res)
      new Mono[M, F](res.setImmutable())
    }
  def adjoint: Mono[M, F] =
    if (lhs.isZero || lhs.isOne) lhs else {
      val res = lhs.data.mutableCopy()
      res.inPlaceAdjoint()
      M.inPlaceNormalForm(res)
      new Mono[M, F](res.setImmutable())
    }
  def pow(rhs: Int): Mono[M, F] = rhs match {
    case 0 => Mono.one[M, F]
    case 1 => lhs
    case i if i > 1 => Vector.fill(i)(lhs).reduce(_ * _)
    case _  => sys.error("Cannot exponentiate to negate powers")
  }

  // we have this === phaseCanonical * phaseOffset
  def phaseOffset: Phase = data.phase
  def phaseCanonical: Mono[M, F] = new Mono[M, F](lhs.data.mutableCopy().setPhase(Phase.one).setImmutable())

  // methods that are valid when F =:= M
  def length(implicit ev: F =:= M): Int = data.length
  def apply(i: Int)(implicit ev: F =:= M): F#Op = data(i)
  def phase(implicit ev: F =:= M): Phase = data.phase
  def mutableCopy(implicit ev: F =:= M): MutableWord[F] = data.mutableCopy

  // to polynomials
  def toPoly: Poly[M, F] = Poly(lhs)
  def +(rhs: Poly[M, F]): Poly[M, F] = lhs.toPoly + rhs
  def *(rhs: Poly[M, F]): Poly[M, F] = lhs.toPoly * rhs
}

object Mono {

  implicit def monoTermToMono[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](monoTerm: MonoTerm[M, F]): Mono[M, F] =
    monoTerm.toMono


  type Free[F <: free.MonoidDef.Aux[F] with Singleton] = Mono[F, F]

  // Factory methods that work for any monoid, free or quotient

  /** Zero monoid element. */
  def zero[M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Mono[M, F] =
    new Mono[M, F](MutableWord.zero[F].setImmutable())

  /** Empty word, i.e. one / the identity. */
  def one[M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Mono[M, F] =
    new Mono[M, F](MutableWord.one[F].setImmutable())

  // all the overloads of apply to workaround the issue shapeless#843

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](): Mono[F, F] = one[F, F]

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase): Mono[F, F] =
    new Mono[F, F](MutableWord[F](phase).setImmutable())

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phasedOp: F#PhasedOp): Mono[F, F] =
    new Mono[F, F](MutableWord[F](phasedOp.phase, Seq(phasedOp.op)).setImmutable())

  // apply(op, op, ...)
  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](op1: F#Op): Mono[F, F] =
    fromSeq(Seq(op1))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](op1: F#Op, op2: F#Op): Mono[F, F] =
    fromSeq(Seq(op1, op2))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](op1: F#Op, op2: F#Op, op3: F#Op): Mono[F, F] =
    fromSeq(Seq(op1, op2, op3))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](op1: F#Op, op2: F#Op, op3: F#Op, op4: F#Op): Mono[F, F] =
    fromSeq(Seq(op1, op2, op3, op4))

  // apply(phase, op, op, ...)

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase, op1: F#Op): Mono[F, F] =
    fromSeq(phase, Seq(op1))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase, op1: F#Op, op2: F#Op): Mono[F, F] =
    fromSeq(phase, Seq(op1, op2))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase, op1: F#Op, op2: F#Op, op3: F#Op): Mono[F, F] =
    fromSeq(phase, Seq(op1, op2, op3))

  def apply[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase, op1: F#Op, op2: F#Op, op3: F#Op, op4: F#Op): Mono[F, F] =
    fromSeq(phase, Seq(op1, op2, op3, op4))

  def fromSeq[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](ops: Seq[F#Op]): Mono[F, F] =
    new Mono[F, F](MutableWord(ops).setImmutable())

  def fromSeq[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](phase: Phase, ops: Seq[F#Op]): Mono[F, F] =
    new Mono[F, F](MutableWord(phase, ops).setImmutable())

  implicit def fromOp[F <: MonoidDef.Aux[F] with Singleton:Witness.Aux](op: F#Op): Mono[F, F] = apply(op)

  // Typeclasses

  implicit def predicate[M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton]: Predicate[Mono[M, F]] = Predicate(gw => !gw.data.isZero)

  implicit def involution[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]): Involution[Mono[M, F]] = (wM.value: M).monoInvolution

  implicit def multiplicativeBinoid[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]): MultiplicativeBinoid[Mono[M, F]] = (wM.value: M).monoMultiplicativeMonoid

  implicit def order[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]): Order[Mono[M, F]] = (wM.value: M).monoOrder

  implicit def genPermAction[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]): Action[Mono[M, F], GenPerm] = (wM.value: M).monoGenPermAction

  implicit def phased[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit wM: Witness.Aux[M]): Phased[Mono[M, F]] = (wM.value: M).monoPhased

}


final class MonoGenPermAction[
  M <: FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](implicit wM: Witness.Aux[M]) extends Action[Mono[M, F], GenPerm] {
  def M: M = wM.value
  def actr(m: Mono[M, F], g: GenPerm): Mono[M, F] = {
    val res = m.data.mutableCopy()
    res.applyGenPermAction(g)
    M.inPlaceNormalForm(res)
    new Mono[M, F](res.setImmutable())
  }
  def actl(g: GenPerm, m: Mono[M, F]): Mono[M, F] = actr(m, g.inverse)
}

final class MonoInstances[
  M <: FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](implicit wM: Witness.Aux[M]) extends MultiplicativeBinoid[Mono[M, F]]
  with Involution[Mono[M, F]]
  with Order[Mono[M, F]]
  {
  implicit def wF: Witness.Aux[F] = (wM.value: M).witnessFree
  def zero: Mono[M, F] = Mono.zero[M, F]
  def one: Mono[M, F] = Mono.one[M, F]
  def adjoint(a: Mono[M, F]): Mono[M, F] = a.adjoint
  def compare(x: Mono[M, F], y: Mono[M, F]): Int = x.compareTo(y)
  def times(x: Mono[M, F], y: Mono[M, F]): Mono[M, F] = x * y

}

final class MonoPhased[
  M <: FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends Phased[Mono[M, F]] {
  def phaseOffset(a: Mono[M, F]): Phase = a.phaseOffset
  def phaseCanonical(a: Mono[M, F]): Mono[M, F] = a.phaseCanonical
  def gtimesl(phase: Phase, m: Mono[M, F]): Mono[M, F] = m * (phase.reciprocal)
  def gtimesr(m: Mono[M, F], phase: Phase): Mono[M, F] = m * phase
}