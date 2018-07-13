package net.alasc.symdpoly.math

import net.alasc.perms.Perm
import spire.algebra.{AbGroup, Action, Eq, Involution}
import spire.syntax.cfor.cforRange
import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

import net.alasc.symdpoly.Phase

/** Phase vector, representing a diagonal matrix with "root of unity" entries
  *
  * Encoding is as follows:
  *
  * elements(0) = N is the number of elements
  * and for i = 0...N, we have
  * elements(2*i + 1) the index
  * elements(2*i + 2) the phase encoding
  */
class Phases(val elements: Array[Int]) extends AnyVal { lhs =>

  def commonRootOrder: Int =
    if (isEmpty) 1 else {
      @tailrec def iter(i: Int, n: Int): Int = // TODO: remove Long after spire.math addition of int-valued lcm/gcd
        if (i == size) n else {
          iter(i + 1, spire.math.lcm(n.toLong, value(i).n.toLong).toInt)
        }
      iter(1, value(0).n)
    }

  override def toString: String = string

  def string: String =
    if (isEmpty) "()" else {
      val sb = StringBuilder.newBuilder
      sb ++= "("
      @tailrec def iter(i: Int, sep: String): Unit =
        if (i < size) {
          sb ++= sep
          sb ++= key(i).toString
          sb ++= " -> "
          sb ++= value(i).toString
          iter(i + 1, ", ")
        }
      iter(0, "")
      sb ++= ")"
      sb.result()
    }

  def hash: Int = {
    var h = MurmurHash3.arraySeed
    cforRange(0 until size) { i =>
      h = MurmurHash3.mix(h, elements(2*i + 1))
      h = MurmurHash3.mix(h, elements(2*i + 2))
    }
    MurmurHash3.finalizeHash(h, size)
  }

  def size: Int = elements(0)
  def isEmpty: Boolean = size == 0
  def key(i: Int): Int = {
    require(i >= 0 && i < size)
    elements(2*i + 1)
  }
  def value(i: Int): Phase = new Phase(valueEncoding(i))
  def valueEncoding(i: Int): Int = {
    require(i >= 0 && i < size)
    elements(2*i + 2)
  }
  def internal_===(rhs: Phases): Boolean = (lhs.size == rhs.size) && {
    @tailrec def check(i: Int): Boolean =
      if (i == lhs.size) true
      else if (lhs.key(i) != rhs.key(i)) false
      else if (lhs.value(i) != rhs.value(i)) false
      else check(i + 1)

    check(0)
  }

  /** Returns a IntPhaseSortedMap with elements (i1 <|+| f, p1), (i2 <|+| f, p2), for
    * the current IntPhasedSortedMap with elements (i1, p1), (i2, p2), ...
    */
  def mapKeys(f: Perm): Phases =
    if (isEmpty) lhs else {
      val n = size
      val newElements = new Array[Int](n * 2 + 1)
      newElements(0) = n
      cforRange(0 until n) { i =>
        newElements(2*i + 1) = f.image(key(i))
        newElements(2*i + 2) = valueEncoding(i)
      }
      @inline def newKey(i: Int): Int = newElements(2*i + 1)
      @inline def swap(i: Int, j: Int): Unit = {
        val tmp1 = newElements(2*i + 1)
        val tmp2 = newElements(2*i + 2)
        newElements(2*i + 1) = newElements(2*j + 1)
        newElements(2*i + 2) = newElements(2*j + 2)
        newElements(2*j + 1) = tmp1
        newElements(2*j + 2) = tmp2
      }
      @tailrec def combSortPass(previousGap: Int): Unit = {
        val gap = spire.math.max(1, (previousGap / 1.247).toInt)
        @tailrec def iter(i: Int, hasSwapped: Boolean): Boolean =
          if (i + gap >= n) hasSwapped else {
            if (newKey(i + gap) < newKey(i)) {
              swap(i + gap, i)
              iter(i + 1, true)
            } else iter(i + 1, hasSwapped)
          }
        if (iter(0, false) || gap > 1)
          combSortPass(gap)
      }
      if (n > 1) combSortPass(n)
      new Phases(newElements)
    }

  def combineRemove(rhs: Phases, remove: Boolean = false): Phases =
    if (rhs.isEmpty) lhs
    else if (lhs.isEmpty && !remove) rhs
    else if (lhs.isEmpty && remove) rhs.inverse
    else {
      val newElements = new Array[Int]((lhs.size + rhs.size) * 2 + 1)
      // walk through the sorted indices, returns the number of stored elements
      @tailrec def rec(i: Int, l: Int, r: Int): Int =
        if (l == lhs.size && r == rhs.size) i // do nothing
        else {
          val li = if (l == lhs.size) Int.MaxValue else lhs.key(l)
          val ri = if (r == rhs.size) Int.MaxValue else rhs.key(r)
          if (li > ri) {
            newElements(2 * i + 1) = rhs.key(r)
            newElements(2 * i + 2) = if (remove) rhs.value(r).reciprocal.encoding else rhs.value(r).encoding
            rec(i + 1, l, r + 1)
          } else if (li < ri) {
            newElements(2 * i + 1) = lhs.key(l)
            newElements(2 * i + 2) = lhs.value(l).encoding
            rec(i + 1, l + 1, r)
          } else { // li == ri and both li < lhs.size and ri < rhs.size
            val newPhase = if (remove) lhs.value(l) / rhs.value(r) else lhs.value(l) * rhs.value(r)
            if (newPhase.isOne)
              rec(i, l + 1, r + 1) // do not store the phase = 1
            else {
              newElements(2 * i + 1) = li
              newElements(2 * i + 2) = newPhase.encoding
              rec(i + 1, l + 1, r + 1)
            }
          }
        }
      val n = rec(0, 0, 0)
      newElements(0) = n
      new Phases(newElements)
    }

  def inverse: Phases = adjoint

  def |+|(rhs: Phases): Phases = combineRemove(rhs, false)

  def |-|(rhs: Phases): Phases = combineRemove(rhs, true)

  def adjoint: Phases =
    if (isEmpty) lhs else {
      val newElements = new Array[Int](lhs.size * 2 + 1)
      newElements(0) = lhs.size
      cforRange(0 until lhs.size) { i =>
        newElements(2*i + 1) = lhs.key(i)
        newElements(2*i + 2) = lhs.value(i).adjoint.encoding
      }
      new Phases(newElements)
    }

  def phaseFor(searchedElement: Int): Phase =
    if (isEmpty) Phase.one else {
      // Invariant: the element, if it is to be found, is found in the interval [left, right] inclusive
      @tailrec def lookup(left: Int, right: Int): Phase =
        if (left > right) Phase.one else {
          val mid = left + (right - left)/2
          val midVal = key(mid)
          if (midVal < searchedElement)
            lookup(mid + 1, right)
          else if (midVal > searchedElement)
            lookup(left, mid - 1)
          else value(mid)
        }
      lookup(0, size - 1)
    }

  /** Returns the Phases that acts only on the elements 0 ... n-1. */
  def truncate(n: Int): Phases =
    if (size == 0 || key(0) >= n) Phases.empty // no phase action on 0..n-1
    else if (key(size - 1) < n) lhs
    else {
      // if we are here, we know that key(0) < n and key(last = size - 1) >= n
      // and thus that size >= 2

      // loop invariant: key(left) < n and key(right) >= n
      @tailrec def findNewSize(left: Int, right: Int): Int =
        if (left + 1 == right) right
        else {
          val mid = left + (right - left)/2
          val midVal = key(mid)
          if (midVal < n)
            findNewSize(mid, right)
          else // midVal >= n
            findNewSize(left, mid)
        }
      val newSize = findNewSize(0, n - 1)
      val newElements = new Array[Int](2*newSize + 1)
      Array.copy(elements, 0, newElements, 0, 2*newSize + 1)
      newElements(0) = newSize
      new Phases(newElements)
    }

  def image(pi: PhasedInt): PhasedInt =
    if (isEmpty) pi else PhasedInt(pi.phase * phaseFor(pi.index), pi.index)

  def invImage(pi: PhasedInt): PhasedInt =
    if (isEmpty) pi else PhasedInt(pi.phase / phaseFor(pi.index), pi.index)

}

object Phases {

  implicit val permAction: Action[Phases, Perm] = new PhasesPermAction

  val empty: Phases = new Phases(Array(0))

  def apply(pairs: (Int, Phase)*): Phases = {
    val reducedPairs = pairs.groupBy(_._1) // group by index
      .mapValues( pairs => pairs.map(_._2).reduce(_ * _) ) // multiply phases
      .filterNot(_._2.isOne) // remove phases = 1
    val n = reducedPairs.size
    val elements = new Array[Int](2*n + 1)
    elements(0) = n
    reducedPairs.keys.toSeq.sorted.zipWithIndex foreach { case (index, i) =>
        elements(2*i + 1) = index
        elements(2*i + 2) = reducedPairs(index).encoding
    }
    new Phases(elements)
  }

  implicit val action: Action[PhasedInt, Phases] = new PhasesPhasedIntAction
  private[this] val instances = new PhasesInstances
  implicit def equ: Eq[Phases] = instances
  implicit def abGroup: AbGroup[Phases] = instances
  implicit def involution: Involution[Phases] = instances

}

final class PhasesPermAction extends Action[Phases, Perm] {
  def actl(g: Perm, p: Phases): Phases = actr(p, g.inverse)
  def actr(p: Phases, g: Perm): Phases = p.mapKeys(g)
}

final class PhasesInstances extends Eq[Phases] with AbGroup[Phases] with Involution[Phases] {
  override def remove(x: Phases, y: Phases): Phases = x |-| y
  def inverse(x: Phases): Phases = x.inverse
  def eqv(x: Phases, y: Phases): Boolean = x.internal_===(y)
  def empty: Phases = Phases.empty
  def adjoint(a: Phases): Phases = a.adjoint
  def combine(x: Phases, y: Phases): Phases = x |+| y
}

final class PhasesPhasedIntAction extends Action[PhasedInt, Phases] {
  def actl(g: Phases, pi: PhasedInt): PhasedInt = g.invImage(pi)
  def actr(pi: PhasedInt, g: Phases): PhasedInt = g.image(pi)
}
