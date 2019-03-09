package net.alasc.symdpoly.evaluation.freebased

import net.alasc.symdpoly.free.MutableWord
import net.alasc.symdpoly.{free, valueOf}
import shapeless.Witness
import spire.algebra.free
import spire.syntax.cfor.cforRange

import scala.annotation.tailrec

/*

/** Scratch pad of mutable words where operations are applied in batch. */
class FreeScratchPad[F <: free.MonoidDef with Singleton: Witness.Aux](var array: Array[MutableWord[F]], var n: Int) { self =>

  def check1(): Unit = {
    cforRange(0 until array.length) {i =>
      cforRange(i + 1 until array.length) { j =>
        assert(array(i) ne array(j))
      }
    }
  }

  def F: F = valueOf[F]

  override def toString: String = {
    val first = (0 until n).map(i => i.toString.padTo(4, ' ') + "* " + array(i).toString + " " + System.identityHashCode(array(i)).toString)
    val second = (n until array.length).map(i => i.toString.padTo(4, ' ') + "  " + array(i).toString + " " + System.identityHashCode(array(i)).toString)
    (first ++ second).mkString("\n")
  }

  // Scratchpad manipulation (resize, clear)

  /** Clears the scratch pad and sets its first element to the given word. */
  def resetWithCopyOf(mono: MutableWord[F]): FreeScratchPad[F] = {
    scratch(0).setToContentOf(mono)
    n = 1
    self
  }

  def scratch(i: Int): MutableWord[F] = {
    if (i >= array.length) ensureMinimalSize(spire.math.max(i + 1, 2 * array.length))
    assert(array(i) ne null)
    array(i)
  }

  def setSize(newSize: Int): Unit = {
    require(newSize >= n)
    if (newSize != n) {
      val newScratch = new Array[free.MutableWord[F]](newSize)
      System.arraycopy(array, 0, newScratch, 0, array.length)
      val reservedSize = if (n == 0) 8 else newScratch(0).reservedSize
      cforRange(array.length until newScratch.length) { i =>
        newScratch(i) = free.MutableWord.one[F](reservedSize)
      }
      array = newScratch
    }
  }

  def ensureMinimalSize(minimalSize: Int): Unit = setSize(spire.math.max(minimalSize, array.length))

  // Sorting and duplicate removal

  /** Sorts the given range from index `start` up to, but not including, the index `end`,
    * while removing duplicates.
    * Operates in place, and returns the range of the result as Some((start1, end1)) with the
    * same convention for the range.
    * Returns None if the monomial is zero, i.e. we found x and x * phase with phase != 1 in the array.
    */
  def insertionSort(start: Int, end: Int): Option[(Int, Int)] = {
    val prev = this.toString
    require(start <= end && start >= 0 && end <= array.length)
    cforRange(start + 1 until end) { i =>
      var item = array(i)
      // Finds the position of array(i)
      @tailrec def findPlace(h: Int): Int =
        if (h <= start) h
        else {
          val c = item.compareToIgnoringPhase(array(h - 1))
          if (c > 0) h // we found the place, bail out
          else if (c == 0 && item.phase != array(h - 1).phase) ~h // the reduced monomial is zero, bail out
          else { // c <= 0
            if (c == 0) item.setToZero() // this is a duplicate, we use zero as a marker of element to remove
            array(h) = array(h - 1)
            findPlace(h - 1)
          }
        }
      val hole = findPlace(i)
      if (hole < 0) {
        array(~hole) = item
        return None
      }
      array(hole) = item
    }
    @tailrec def findNewStart(i: Int): Int =
      if (i == end) {
        sys.error("Should never happen")
      }
      else if (array(i).isZero) findNewStart(i + 1)
      else i
    Some((findNewStart(start), end))
  }

  /** Removes duplicates in the scratch array, and returns whether the resulting monomial is zero. */
  def removeDuplicatesAndCheckForZero(): Boolean = {
    // TODO: implement merge sort with duplicate removal
    // See Bitton, Dina, and David J. DeWitt. “Duplicate Record Elimination in Large Data Files.”
    // ACM Trans. Database Syst. 8, no. 2 (June 1983): 255–265. https://doi.org/10.1145/319983.319987.
    insertionSort(0, n) match {
      case None =>
        true // the monomial is zero
      case Some((newStart, newEnd)) =>
        val newN = newEnd - newStart
        if (newStart != 0) {
          cforRange(0 until newN) { i =>
            val tmp = array(i)
            array(i) = array(newStart + i)
            array(newStart + i) = tmp
          }
        }
        n = newN
        false // the monomial is not (yet) proven to be zero
    }
  }

}

object FreeScratchPad {
  def apply[F <: free.MonoidDef with Singleton:Witness.Aux]: FreeScratchPad[F] = new FreeScratchPad[F](Array.empty[free.MutableWord[F]], 0)
}

*/
