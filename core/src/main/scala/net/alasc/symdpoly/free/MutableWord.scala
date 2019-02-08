package net.alasc.symdpoly.free

import net.alasc.symdpoly.math.{GenPerm, PhasedInt}
import shapeless.Witness
import spire.algebra.Order
import spire.syntax.cfor.cforRange
import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

import net.alasc.symdpoly.Phase

/** A mutable word in a graded, pointed monoid.
  *
  * @param phase The phase exp(2*pi*grade*i/M.grade)
  * @param length Length of the word; special convention: length = -1 for the zero word, in which case phase = Phase.one
  * @param indices Indices of the word letters
  */
class MutableWord[F <: MonoidDef with Singleton](var phase: Phase, var length: Int, var indices: Array[Int], var mutable: Boolean)
                                                (implicit val wF: Witness.Aux[F]) {
  lhs =>

  def reservedSize: Int = indices.length

  def setToContentOf(rhs: MutableWord[F]): MutableWord[F] = {
    lhs.ensureLength(rhs.length)
    lhs.phase = rhs.phase
    lhs.length = rhs.length
    if (lhs.length > 0)
      Array.copy(rhs.indices, 0, lhs.indices, 0, rhs.length)
    lhs
  }

  def mutableCopy(): MutableWord[F] =
    new MutableWord[F](phase, length, indices.clone, true)

  def mutableCopy(newReservedLength: Int): MutableWord[F] = {
    require(newReservedLength >= length)
    val newIndices = new Array[Int](newReservedLength)
    Array.copy(indices, 0, newIndices, 0, length)
    new MutableWord[F](phase, length, newIndices, true)
  }

  /** Creates an immutable copy if this mutable word is mutable, otherwise simply returns the existing immutable word. */
  def immutableCopy: MutableWord[F] =
    if (!mutable) this
    else if (isZero) F.immutableMutableWordZero
    else if (isOne) F.immutableMutableWordOne
    else if (isMinusOne) F.immutableMutableWordMinusOne
    else {
      val newIndices = new Array[Int](length)
      Array.copy(indices, 0, newIndices, 0, length)
      new MutableWord[F](phase, length, newIndices, false)
    }

  def check(): Unit = {
    require(length != -1 || phase == Phase.one)
    require(length >= -1)
    require(length <= indices.length)
  }

  check()

  def setImmutable(): MutableWord[F] = {
    mutable = false
    lhs
  }

  def checkMutable(): Unit = {
    assert(mutable)
  }

  override def equals(any: Any): Boolean = any match {
    case rhs: MutableWord[F] if lhs.F eq rhs.F =>
      if (lhs.length == -1 && rhs.length == -1) true else
      (lhs.phase == rhs.phase) && (lhs.length == rhs.length) && {
        cforRange(0 until lhs.length) { i =>
          if (lhs.indices(i) != rhs.indices(i)) return false
        }
        true
      }
  }

  def wordString: String = Seq.tabulate(length)(i => F.opFromIndex(indices(i))).mkString(" ")

  override def toString: String =
    if (isZero) "0"
    else if (isEmpty) phase.toString
    else if (phase.isOne) wordString
    else if (phase.isMinusOne) "- " + wordString
    else phase.toString + " " + wordString

  def computeHash: Int = if (isZero) 0 else {
    var h = MurmurHash3.arraySeed
    h = MurmurHash3.mix(h, phase.encoding)
    cforRange(0 until length) { i =>
      h = MurmurHash3.mix(h, indices(i))
    }
    MurmurHash3.finalizeHash(h, length)
  }

  // cache hashCode as it is used repeatedly in polynomial computations
  lazy val immutableHash: Int = {
    assert(!lhs.mutable)
    computeHash
  }

  override def hashCode: Int = if (mutable) computeHash else immutableHash

  def F: F = wF.value

  def isZero: Boolean = length == -1
  def isEmpty: Boolean = length == 0
  def isOne: Boolean = isEmpty && phase.isOne
  def isMinusOne: Boolean = isEmpty && phase.isMinusOne

  def compareToIgnoringPhase(rhs: MutableWord[F]): Int =
    if (lhs.isZero && rhs.isZero) 0
    else if (lhs.isZero && !rhs.isZero) -1
    else if (!lhs.isZero && rhs.isZero) 1
    else {
      if (lhs.length < rhs.length) -1
      else if (lhs.length > rhs.length) 1
      else {
        @tailrec def rec(i: Int): Int =
          if (i == length) 0
          else {
            val c = java.lang.Integer.signum(lhs.indices(i) - rhs.indices(i))
            if (c != 0) c else rec(i + 1)
          }
        rec(0)
      }
    }

  def compareTo(rhs: MutableWord[F]): Int = lhs.compareToIgnoringPhase(rhs) match {
    case 0 if lhs.isZero => 0
    case 0 => lhs.phase.compareTo(rhs.phase)
    case c => c
  }

  def ensureLength(newMaxLength: Int): Unit =
    if (newMaxLength > indices.length) {
      val newIndices = new Array[Int](newMaxLength)
      Array.copy(indices, 0, newIndices, 0, length)
      indices = newIndices
    }

  def setPhase(newPhase: Phase): MutableWord[F] = {
    checkMutable()
    phase = newPhase
    lhs
  }

  def setToZero(): MutableWord[F] = {
    checkMutable()
    phase = Phase.one
    length = -1
    lhs
  }

  def flipSign(): MutableWord[F] = {
    checkMutable()
    if (!lhs.isZero) phase = -phase
    lhs
  }

  def multiplyBySignOf(rhs: Int): MutableWord[F] = {
    checkMutable()
    if (!lhs.isZero) rhs.signum match {
      case -1 => flipSign()
      case 0 => setToZero()
      case _ => // 1
    }
    lhs
  }

  def *=(rhs: Phase): MutableWord[F] = {
    checkMutable()
    if (!lhs.isZero) phase *= rhs
    lhs
  }

  def *=(rhs: F#Op): MutableWord[F] = {
    checkMutable()
    if (lhs.isZero) return lhs
    ensureLength(length + 1)
    indices(length) = F.indexFromOp(rhs)
    length += 1
    lhs
  }

  def *=(rhs: MutableWord[F]): MutableWord[F] =
    if (lhs.isZero) lhs
    else if (rhs.isZero) setToZero
    else {
      checkMutable()
      ensureLength(lhs.length + rhs.length)
      Array.copy(rhs.indices, 0, lhs.indices, lhs.length, rhs.length)
      lhs.length += rhs.length
      lhs.phase *= rhs.phase
      lhs
    }

  def apply(i: Int): F#Op = {
    require(i >= 0 && i < length)
    F.opFromIndex(indices(i))
  }

  def update(i: Int, op: F#Op): Unit = update(i, F.indexFromOp(op))

  def update(i: Int, index: Int): Unit = {
    require(i >= 0)
    require(i <= length)
    checkMutable()
    if (i == length) {
      ensureLength(length + 1)
      length += 1
    }
    indices(i) = index
  }

  def swap(i: Int, j: Int): MutableWord[F] = {
    checkMutable()
    require(i >= 0 && j >= 0 && i <= length && j <= length)
    val tmp = indices(j)
    indices(j) = indices(i)
    indices(i) = tmp
    lhs
  }

  def inPlaceAdjoint(): MutableWord[F] = {
    phase = phase.adjoint
    @tailrec def iter(left: Int, right: Int): Unit =
      if (left == right) {
        val i = indices(left)
        val op = F.indexAdjoint(i)
        update(left, op)
      } else if (left < right) {
        val li = indices(left)
        val ri = indices(right)
        val lop = F.indexAdjoint(li)
        val rop = F.indexAdjoint(ri)
        update(left, rop)
        update(right, lop)
        iter(left + 1, right - 1)
      }
    iter(0, length - 1)
    lhs
  }

  def applyGenPermAction(rhs: GenPerm): MutableWord[F] = if (length == -1) lhs else {
    @tailrec def iter(i: Int, currentPhase: Phase): Unit =
      if (i == length)
        phase = currentPhase
      else {
        val image = rhs.image(PhasedInt(currentPhase, indices(i)))
        val newPhase = image.phase
        val newIndex = image.index
        indices(i) = newIndex
        iter(i + 1, newPhase)
      }
    iter(0, phase)
    lhs
  }

}

object MutableWord {

  def one[F <: MonoidDef with Singleton:Witness.Aux]: MutableWord[F] = empty[F](0)

  def empty[F <: MonoidDef with Singleton:Witness.Aux](reservedLength: Int): MutableWord[F] =
    new MutableWord[F](Phase.one, 0, new Array[Int](reservedLength), true)

  def zero[F <: MonoidDef with Singleton:Witness.Aux]: MutableWord[F] =
    new MutableWord(Phase.one, -1, new Array[Int](0), true)

  def apply[F <: MonoidDef with Singleton: Witness.Aux](ops: Seq[F#Op]): MutableWord[F] = {
    val mgw = empty[F](ops.length)
    ops.foreach { op => mgw *= op }
    mgw
  }

  def apply[F <: MonoidDef with Singleton: Witness.Aux](phase: Phase, ops: Seq[F#Op]): MutableWord[F] = {
    val mgw = empty[F](ops.length)
    ops.foreach { op => mgw *= op }
    mgw.setPhase(phase)
    mgw
  }

  def apply[F <: MonoidDef with Singleton: Witness.Aux](phase: Phase): MutableWord[F] =
    new MutableWord(phase, 0, new Array[Int](0), true)

  implicit def order[F <: MonoidDef with Singleton](implicit wM: Witness.Aux[F]): Order[MutableWord[F]] =
    (wM.value: F).mutableWordOrder

}

final class MutableWordOrder[F <: MonoidDef with Singleton] extends Order[MutableWord[F]] {
  def compare(x: MutableWord[F], y: MutableWord[F]): Int = x.compareTo(y)
}
