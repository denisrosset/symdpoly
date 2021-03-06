package net.alasc.symdpoly.free

import net.alasc.symdpoly.math.{GenPerm, Phase, PhasedInt}
import shapeless.Witness
import spire.algebra.Order
import spire.syntax.cfor.cforRange

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3
import java.util.Arrays

/** A mutable word in a free monoid. The word can be made immutable when all required transformations have been realized.
  *
  * @param phase The phase exp(2*pi*grade*i/M.grade)
  * @param length Length of the word; special convention: length = -1 for the zero word, in which case phase = Phase.one
  * @param indices Indices of the word letters
  * @param state   Whether the word is Mutable, Locked or Immutable (MUTATION_IMMUTABLE)
  *                 The Locked state caches the hashCode value and prevents modification. Locked words cannot be updated,
  *                 but can be reset to an empty mutable state.
  * @param cachedHash If != -1, precomputed hash value. If == -1, no precomputed hash value is known.
  *                   Note that -1 is used as a marker for unknown hash, and we make sure that the computed hashCode of MutableWord
  *                   is never -1.
  */
class MutableWord[F <: MonoDef with Singleton](var phase: Phase,
                                               var length: Int,
                                               var indices: Array[Int],
                                               var state: MutableWord.State,
                                               var cachedHash: Int)
                                              (implicit val wF: Witness.Aux[F]) {
  lhs =>

  import MutableWord.{Mutable, Locked, Immutable}

  def F: F = wF.value

  //region Class invariants

  /** Check class invariants */
  def check(): Unit = {
    require(length != -1 || phase == Phase.one)
    require(length >= -1)
    require(length <= indices.length)
  }

  check() // check invariants at construction time

  //endregion

  //region Java methods

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

  def computeHash: Int = if (isZero) 0 else {
    var h = MurmurHash3.arraySeed
    h = MurmurHash3.mix(h, phase.encoding)
    cforRange(0 until length) { i =>
      h = MurmurHash3.mix(h, indices(i))
    }
    val res = MurmurHash3.finalizeHash(h, length)
    if (res == -1) 0 else res
  }

  override def hashCode: Int = state match {
    case Locked | Immutable =>
      if (cachedHash == -1)
        cachedHash = computeHash
      cachedHash
    case Mutable => computeHash
  }

  def wordString: String = Seq.tabulate(length)(i => F.opFromIndex(indices(i))).mkString(" ")

  override def toString: String =
    if (isZero) "0"
    else if (isEmpty) phase.toString
    else if (phase.isOne) wordString
    else if (phase.isMinusOne) "- " + wordString
    else phase.toString + " " + wordString

  //endregion

  //region Methods valid for both mutable and immutable words

  def isZero: Boolean = length == -1

  /** True if the length of this word is zero letters (and the zero word is not considered empty). */
  def isEmpty: Boolean = length == 0

  def isOne: Boolean = isEmpty && phase.isOne

  def isMinusOne: Boolean = isEmpty && phase.isMinusOne

  /** Returns the i-th letter of this word. */
  def apply(i: Int): F#Op = {
    require(i >= 0 && i < length)
    F.opFromIndex(indices(i))
  }

  /** Total order on words, phase being ignored. */
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

  /** Total order on words with a phase; the words are compared first, and then the phases. */
  def compareTo(rhs: MutableWord[F]): Int = lhs.compareToIgnoringPhase(rhs) match {
    case 0 if lhs.isZero => 0
    case 0 => lhs.phase.compareTo(rhs.phase)
    case c => c
  }

  /** Creates an immutable copy if this mutable word is mutable, otherwise simply returns the existing immutable word. */
  def immutableCopy: MutableWord[F] =
    if (state == Immutable) this
    else if (isZero) F.immutableMutableWordZero
    else if (isOne) F.immutableMutableWordOne
    else if (isMinusOne) F.immutableMutableWordMinusOne
    else {
      val newIndices = new Array[Int](length)
      System.arraycopy(indices, 0, newIndices, 0, length)
      new MutableWord[F](phase, length, newIndices, Immutable, if (state != Mutable) cachedHash else -1)
    }

  /** Returns a mutable copy of this word. */
  def mutableCopy(): MutableWord[F] =
    new MutableWord[F](phase, length, indices.clone, Mutable, -1)

  /** Returns a mutable copy of this word with the given reserved size in the created word. */
  def mutableCopy(newReservedLength: Int): MutableWord[F] = {
    require(newReservedLength >= length)
    val newIndices = new Array[Int](newReservedLength)
    System.arraycopy(indices, 0, newIndices, 0, length)
    new MutableWord[F](phase, length, newIndices, Mutable, -1)
  }

  //endregion

  //region Mutability management

  def reservedSize: Int = indices.length

  /** Reserves size in the mutable indices array. */
  def ensureLength(newMaxLength: Int): Unit =
    if (newMaxLength > indices.length) {
      val newIndices = new Array[Int](newMaxLength)
      System.arraycopy(indices, 0, newIndices, 0, length)
      indices = newIndices
    }

  /** Sets the contents of this [[MutableWord]] to the contents of the given word. */
  def setToContentOf(rhs: MutableWord[F]): MutableWord[F] = {
    lhs.ensureLength(rhs.length)
    lhs.phase = rhs.phase
    lhs.length = rhs.length
    if (lhs.length > 0)
      System.arraycopy(rhs.indices, 0, lhs.indices, 0, rhs.length)
    lhs
  }

  /** Makes a [[MutableWord]] immutable, so it can be stored as an immutable object. */
  def setImmutable(): MutableWord[F] = {
    state = Immutable
    lhs
  }

  /** Asserts that this [[MutableWord]] is mutable. Called before mutable operations are performed. */
  def checkMutable(): Unit = {
    assert(state == Mutable)
  }

  def lock(): Unit = {
    assert(state != Immutable)
    state = Locked
  }

  def reset(): Unit = {
    assert(state != Immutable)
    state = Mutable
    phase = Phase.one
    length = 0
    cachedHash = -1
  }

  //endregion

  //region Operations on a mutable word

  /** Updates the operator variable at the position i. */
  def update(i: Int, op: F#Op): Unit = update(i, F.indexFromOp(op))

  /** Updates the operator variable at the position i by the operator variable with given index. */
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

  /** Removes an operator variable at the specified position, shifting the position of the variables after it, and
    * updating the mutable word length. */
  def remove(i: Int): MutableWord[F] = removeRange(i, i + 1)

  /** Removes the letters in the range [from, until[, until not included. */
  def removeRange(from: Int, until: Int): MutableWord[F] = {
    require(0 <= from && from < length && from <= until && until <= length)
    checkMutable()
    if (from == until) return lhs // no op, bail out early
    if (length - until > 0) // if something remains on the right of the removed range
      System.arraycopy(indices, until, indices, from, length - until)
    length -= (until - from)
    lhs
  }

  /** Inserts the given word at the specified position i.
    *
    * If the inserted word has a phase, the lhs word phase is multiplied by it.
    * If the inserted word is zero, the lhs word is set to zero.
    */
  def insert(i: Int, toInsert: MutableWord[F]): MutableWord[F] = {
    require(i >= 0 && i < length)
    checkMutable()
    if (toInsert.isZero) return setToZero() // special case, bail out
    lhs *= toInsert.phase
    lhs.ensureLength(lhs.length + toInsert.length)
    if (toInsert.length > 0) {
      if (i != length)
        System.arraycopy(indices, i, indices, i + toInsert.length, length - i)
      System.arraycopy(toInsert.indices, 0, indices, i, toInsert.length)
      length += toInsert.length
    }
    lhs
  }

  /** Rotates the operators in this word by one position to the right. */
  def rotateRight(): MutableWord[F] = {
    val last = indices(length - 1)
    System.arraycopy(indices, 0, indices, 1, length - 1)
    indices(0) = last
    lhs
  }

  /** Rotates the operators in this word by one position to the right. */
  def rotateLeft(): MutableWord[F] = {
    val first = indices(0)
    System.arraycopy(indices, 1, indices, 0, length - 1)
    indices(length - 1) = first
    lhs
  }

  /** Replaces the range [from, until[ by the given word.
    *
    * Is equivalent to removeRange(from, until) followed by insert(from, replaceBy).
    */
  def replaceRange(from: Int, until: Int, replaceBy: MutableWord[F]): MutableWord[F] = {
    require(0 <= from && from < length && from <= until && until <= length)
    checkMutable()
    if (replaceBy.isZero) return setToZero() // special case, bail out
    lhs *= replaceBy.phase
    lhs.ensureLength(lhs.length - (until - from) + replaceBy.length)
    if (until != length && (until - from) != replaceBy.length)
      System.arraycopy(indices, until, indices, from + replaceBy.length, length - until)
    if (replaceBy.length > 0)
      System.arraycopy(replaceBy.indices, 0, indices, from, replaceBy.length)
    length += replaceBy.length - (until - from)
    lhs
  }

  /** Sets this mutable word to the zero monomial. */
  def setToZero(): MutableWord[F] = {
    checkMutable()
    phase = Phase.one
    length = -1
    lhs
  }

  /** Sets the phase of this mutable word. */
  def setPhase(newPhase: Phase): MutableWord[F] = {
    checkMutable()
    phase = newPhase
    lhs
  }

  /** Flips the phase, i.e. sets phase -> -phase on this mutable word. */
  def flipSign(): MutableWord[F] = {
    checkMutable()
    if (!lhs.isZero) phase = -phase
    lhs
  }

  /** Multiplies the phase of this word by the sign of the given integer. When rhs = 0, sets this word to the zero word. */
  def multiplyBySignOf(rhs: Int): MutableWord[F] = {
    checkMutable()
    if (!lhs.isZero) rhs.signum match {
      case -1 => flipSign()
      case 0 => setToZero()
      case _ => // 1
    }
    lhs
  }

  /** Multiplies this mutable word by the given phase. */
  def *=(rhs: Phase): MutableWord[F] = {
    checkMutable()
    if (!lhs.isZero) phase *= rhs
    lhs
  }

  /** Multiplies this mutable word by the given operator variable. */
  def *=(rhs: F#Op): MutableWord[F] = {
    checkMutable()
    if (lhs.isZero) return lhs
    ensureLength(length + 1)
    indices(length) = F.indexFromOp(rhs)
    length += 1
    lhs
  }

  /** Multiplies this mutable word by the given word. */
  def *=(rhs: MutableWord[F]): MutableWord[F] =
    if (lhs.isZero) lhs
    else if (rhs.isZero) setToZero
    else {
      checkMutable()
      ensureLength(lhs.length + rhs.length)
      System.arraycopy(rhs.indices, 0, lhs.indices, lhs.length, rhs.length)
      lhs.length += rhs.length
      lhs.phase *= rhs.phase
      lhs
    }

  /** Swaps letters at the i-th and j-th position in this mutable word. */
  def swap(i: Int, j: Int): MutableWord[F] = {
    checkMutable()
    require(i >= 0 && j >= 0 && i <= length && j <= length)
    val tmp = indices(j)
    indices(j) = indices(i)
    indices(i) = tmp
    lhs
  }

  /** Sets this word to its adjoint. */
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

  /** Applies the given generalized permutation to this word in place. */
  def inPlaceGenPermAction(rhs: GenPerm): MutableWord[F] = if (length == -1) lhs else {
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

  //endregion

}

object MutableWord {

  sealed trait State
  case object Mutable extends State
  case object Locked extends State
  case object Immutable extends State

  /** Constructs a mutable identity word. */
  def one[F <: MonoDef with Singleton:Witness.Aux]: MutableWord[F] = one[F](0)

  /** Constructs a mutable identity word with reserved size. */
  def one[F <: MonoDef with Singleton:Witness.Aux](reservedLength: Int): MutableWord[F] =
    new MutableWord[F](Phase.one, 0, new Array[Int](reservedLength), Mutable, -1)

  /** Constructs a mutable zero word. */
  def zero[F <: MonoDef with Singleton:Witness.Aux]: MutableWord[F] =
    new MutableWord(Phase.one, -1, new Array[Int](0), Mutable, -1)

  /** Constructs a mutable word from a sequence of operators. */
  def apply[F <: MonoDef with Singleton: Witness.Aux](ops: Seq[F#Op]): MutableWord[F] = {
    val mgw = one[F](ops.length)
    ops.foreach { op => mgw *= op }
    mgw
  }

  /** Constructs a mutable word from a phase and a sequence of operators. */
  def apply[F <: MonoDef with Singleton: Witness.Aux](phase: Phase, ops: Seq[F#Op]): MutableWord[F] = {
    val mgw = one[F](ops.length)
    ops.foreach { op => mgw *= op }
    mgw.setPhase(phase)
    mgw
  }

  /** Constructs a mutable word that contains a scalar phase. */
  def apply[F <: MonoDef with Singleton: Witness.Aux](phase: Phase): MutableWord[F] =
    new MutableWord(phase, 0, new Array[Int](0), Mutable, -1)

  /** Order instance for mutable words. */
  implicit def order[F <: MonoDef with Singleton](implicit wM: Witness.Aux[F]): Order[MutableWord[F]] =
    (wM.value: F).mutableWordOrder

}

