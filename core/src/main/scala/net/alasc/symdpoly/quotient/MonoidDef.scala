package net.alasc.symdpoly
package quotient
import net.alasc.algebra.PermutationAction
import net.alasc.symdpoly.free.{FreePermutation, MutablePoly, MutableWord, QuotientPermutation}
import net.alasc.symdpoly.generic.FreeBasedMonoidDef
import spire.syntax.cfor._

import scala.annotation.tailrec
import net.alasc.symdpoly
import net.alasc.symdpoly.Phase
import net.alasc.symdpoly.math.GenPerm
import net.alasc.util._

abstract class MonoidDef extends FreeBasedMonoidDef {
  monoidDef =>

  def pairRules: PairRules[Free]

  lazy val (monoSet, action, partition) = {
    val m = pairRules.nRootsOfUnity
    val n = Free.nOperators
    def op(i: Int): Free#Op = Free.opFromIndex(i)
    def monoFromOpIndex(i: Int): Mono[Free, Free] = Mono(op(i))
    val phases: Vector[Phase] = Vector.tabulate(m)(k => Phase(k, m))
    val monos1: Vector[Mono[Free, Free]] = Vector.tabulate(n)(i => monoFromOpIndex(i))
    val monos2: Vector[Mono[Free, Free]] = Vector.tabulate(n, n)( (i, j) => Mono(op(i), op(j)) ).flatten
    val monos: Vector[Mono[Free, Free]] = Vector(Mono.one[Free, Free]) ++ (monos1 ++ monos2).flatMap(m => phases.map(p => m * p))
    val monoSet: OrderedSet[Mono[Free, Free]] = OrderedSet.fromUnique(monos)
    val action = new PermutationAction[FreePermutation[Free]] {
      def isFaithful: Boolean = true
      def findMovedPoint(g: FreePermutation[Free]): NNOption = g.genPerm.largestMovedPoint match {
        case NNOption(i) => NNSome(monoSet.indexOf(monoFromOpIndex(i)))
        case _ => NNNone
      }
      def movedPointsUpperBound(g: FreePermutation[Free]): NNOption = NNSome(monoSet.length - 1)
      def actl(g: FreePermutation[Free], i: Int): Int = actr(i, g.inverse)
      def actr(i: Int, g: FreePermutation[Free]): Int = monoSet.indexOf(Free.monoGenPermAction.actr(monoSet(i), g))
    }


  }
  /*
    val grp = GenPerm.generalizedSymmetricGroup(m, n)
    val normalForms = monoSet.iterator.map(self.quotient(_)).toVector
    val partition = Partition.fromSeq(normalForms)
    grp.unorderedPartitionStabilizer(action, partition)
  }

   */
  def quotient(permutation: FreePermutation[Free]): Option[QuotientPermutation[this.type]] = {

  }

  def quotient(word: Mono[Free, Free]): Monomial = {
    val res = word.data.mutableCopy()
    inPlaceNormalForm(res)
    new Mono[monoidDef.type, Free](res.setImmutable())
  }

  def quotient(poly: Poly[Free, Free]): Poly[monoidDef.type, Free] =
    if (poly.nTerms == 0) symdpoly.Poly.zero[monoidDef.type, Free] else {
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

  def inPlaceNormalForm(word: MutableWord[Free], start: Int = 0): Boolean =
    if (word.isZero) false else {
      @tailrec def rec(i: Int, n: Int, modified: Boolean): Boolean =
        if (i < 0)
          rec(0, n, modified)
        else if (i >= n - 1) {
          word.length = n
          modified
        } else {
          val tailSize = n - i - 2
          val oi1 = word.indices(i)
          val oi2 = word.indices(i + 1)
          pairRules.rule(oi1, oi2) match {
            case PairRules.RemoveBoth => // discard x_i and x_i+1
              Array.copy(word.indices, i + 2, word.indices, i, tailSize) // move back two places the tail elements
              rec(i - 1, n - 2, true) // go back one element to check for possible new substitutions
            case PairRules.KeepFirst =>
              Array.copy(word.indices, i + 2, word.indices, i + 1, tailSize) // move back one place the tail elements
              rec(i, n - 1, true)
            case PairRules.Swap =>
              word.swap(i, i + 1) // swap x_i and x_i+1
              rec(i - 1, n, true) // go back one element to check for possible new substitutions
            case PairRules.SetToZero =>
              // the monomial was not zero before
              word.setToZero()
              true
            case PairRules.Preserve => rec(i + 1, n, modified) // both elements are good, move to next
            case PairRules.Custom =>
              val result = pairRules.custom(Free.opFromIndex(oi1) -> Free.opFromIndex(oi2))
              if (result.isZero) {
                word.setToZero()
                true
              } else {
                word *= result.phase // multiply phase
                result.length match {
                  case 0 => // as in PairRules.RemoveBoth
                    Array.copy(word.indices, i + 2, word.indices, i, tailSize) // move back two places
                    rec(i - 1, n - 2, true)
                  case 1 =>
                    Array.copy(word.indices, i + 2, word.indices, i + 1, tailSize)
                    word.indices(i) = result.data.indices(0)
                    rec(i - 1, n - 1, true)
                  case 2 =>
                    word.indices(i) = result.data.indices(0)
                    word.indices(i + 1) = result.data.indices(1)
                    rec(i - 1, n, true)
                  case _ =>
                    throw new IllegalArgumentException(s"Rules cannot grow the word size")
                }

              }
          }
        }
      rec(start, word.length, false)
    }
}

object MonoidDef {
  def apply[F <: free.MonoidDef.Aux[F] with Singleton](f: F)(pairSubstitutions: PairSubstitutions[F]): quotient.MonoidDef.Aux[F] =
    new MonoidDef {
      type Free = F
      def Free: F = f
      val pairRules: PairRules[Free] = PairRules(pairSubstitutions)
    }
  type Aux[F <: free.MonoidDef with Singleton] = quotient.MonoidDef { type Free = F }

}

/*
package net.alasc.sdpoly

import shapeless.Witness
import spire.algebra._

import scala.annotation.tailrec

trait FastQuotientMonoid extends GenMonoid { self =>
  type F <: FreeMonoid with Singleton
  val F: F
  val fastSubs: FastSubs[F]
  val name: String

  implicit def witnessF: Witness.Aux[F] = (F: F).witness

  /** Performs in place substitution in the given mutable monomial.
    *
    * Returns whether the monomial has been modified.
    */
  def subsInPlace(mutableMono: MutableMono[F], start: Int = 0): Boolean =
    if (mutableMono.isZero) false else {
      @tailrec def rec(i: Int, n: Int, modified: Boolean): Boolean =
        if (i < 0)
          rec(0, n, modified)
        else if (i >= n - 1) {
          mutableMono.setLength(n)
          modified
        } else {
          val tailSize = n - i - 2
          fastSubs(mutableMono(i), mutableMono(i + 1)) match {
            case FastSubs.Discard => // discard x_i and x_i+1
              mutableMono.copyRangeTo(i + 2, mutableMono, i, tailSize) // move back two places the tail elements
              rec(i - 1, n - 2, true) // go back one element to check for possible new substitutions
            case FastSubs.KeepFirst =>
              mutableMono.copyRangeTo(i + 2, mutableMono, i + 1, tailSize) // move back one place the tail elements
              rec(i, n - 1, true)
            case FastSubs.Swap =>
              mutableMono.swap(i, i + 1) // swap x_i and x_i+1
              rec(i - 1, n, true) // go back one element to check for possible new substitutions
            case FastSubs.Zero =>
              // the monomial was not zero before
              mutableMono.setZero()
              true
            case FastSubs.Keep => rec(i + 1, n, modified) // both elements are good, move to next
          }
        }
      rec(start, mutableMono.length, false)
    }

  def subsInPlaceSigned(mutableSignedMono: MutableSignedMono[F], start: Int = 0): Boolean =
    if (mutableSignedMono.isZero) false else {
      @tailrec def rec(i: Int, n: Int, modified: Boolean): Boolean =
        if (i < 0)
          rec(0, n, modified)
        else if (i >= n - 1) {
          mutableSignedMono.setLength(n)
          modified
        } else {
          val tailSize = n - i - 2
          fastSubs(mutableSignedMono(i), mutableSignedMono(i + 1)) match {
            case FastSubs.Discard => // discard x_i and x_i+1
              mutableSignedMono.copyRangeTo(i + 2, mutableSignedMono, i, tailSize) // move back two places the tail elements
              rec(i - 1, n - 2, true) // go back one element to check for possible new substitutions
            case FastSubs.KeepFirst =>
              mutableSignedMono.copyRangeTo(i + 2, mutableSignedMono, i + 1, tailSize) // move back one place the tail elements
              rec(i, n - 1, true)
            case FastSubs.Swap =>
              mutableSignedMono.swap(i, i + 1) // swap x_i and x_i+1
              rec(i - 1, n, true) // go back one element to check for possible new substitutions
            case FastSubs.Zero =>
              // the monomial was not zero before
              mutableSignedMono.setZero()
              true
            case FastSubs.Keep => rec(i + 1, n, modified) // both elements are good, move to next
          }
        }
      rec(start, mutableSignedMono.length, false)
    }

  def castResult(mutableMono: MutableMono[F]): Mono[self.type] =
    mutableMono.monoResult().unsafeCast(self)

  def castCopy(mutableMono: MutableMono[F]): Mono[self.type] =
    mutableMono.monoCopy.unsafeCast(self)

  def rewriteResult(mutableMono: MutableMono[F]): Mono[self.type] = {
    subsInPlace(mutableMono)
    mutableMono.monoResult().unsafeCast(self)
  }

  def rewriteCopy(mutableMono: MutableMono[F]): Mono[self.type] = {
    val res = mutableMono.mutableMonoCopy()
    subsInPlace(res)
    res.monoResult().unsafeCast(self)
  }

  def rewriteResult(mutableMono: MutableSignedMono[F]): SignedMono[self.type] = {
    val sign = mutableMono.sign
    val mm = mutableMono.withoutSign()
    subsInPlace(mm)
    val msm = mm.withSign()
    msm.setSign(sign)
    msm.signedMonoResult().unsafeCast(self)
  }

  def rewriteCopy(mutableMono: MutableSignedMono[F]): SignedMono[self.type] = {
    val sign = mutableMono.sign
    val mm = mutableMono.mutableMonoCopy()
    subsInPlace(mm)
    val msm = mm.withSign()
    msm.setSign(sign)
    msm.signedMonoResult().unsafeCast(self)
  }

  def apply(mono: Mono[F]): Mono[self.type] = rewriteResult(mono.mutableMonoCopy)

  def apply(mono: SignedMono[F]): SignedMono[self.type] = rewriteResult(mono.mutableSignedMonoCopy)

  def apply[K:Eq:Field:Involution](poly: Poly[F, K]): Poly[self.type, K] = {
    import spire.compat.ordering
    Poly.fromMap(poly.toMap.map { case (k, v) => apply(k) -> v })
  }

  def preimage(mono: Mono[self.type]): Mono[F] = mono.unsafeCast(F: F)

  def preimage(mono: SignedMono[self.type]): SignedMono[F] = mono.unsafeCast(F: F)

  lazy val monoTypeclasses: Involution[Mono[self.type]] with MultiplicativeMonoid[Mono[self.type]] =
    new Involution[Mono[self.type]] with MultiplicativeMonoid[Mono[self.type]] {
      def adjoint(mono: Mono[self.type]): Mono[self.type] = {
        val mm = preimage(mono).mutableMonoCopy()
        mm.inPlaceAdjoint()
        rewriteResult(mm)
      }

      def one: Mono[self.type] = Mono.one[self.type]

      def times(x: Mono[self.type], y: Mono[self.type]): Mono[self.type] =
        if (x.isZero) x
        else if (y.isZero) y
        else if (x.isOne) y
        else if (y.isOne) x
        else {
          val l = x.length + y.length
          val mutableMono = MutableMono.empty[F](l)
          preimage(x).copyRangeTo(0, mutableMono, 0, x.length)
          preimage(y).copyRangeTo(0, mutableMono, x.length, y.length)
          mutableMono.setLength(l)
          rewriteResult(mutableMono)
        }
    }

}

object FastQuotientMonoid {
  type Aux[F0 <: FreeMonoid with Singleton] = FastQuotientMonoid { type F = F0 }

  def apply[F0 <: FreeMonoid with Singleton](F0: F0)(fastSubs0: FastSubs[F0])(implicit n: sourcecode.Name): FastQuotientMonoid.Aux[F0] =
    new FastQuotientMonoid {
      type F = F0
      val F: F = F0
      val fastSubs: FastSubs[F] = fastSubs0
      val name: String = n.value
    }

}
 */