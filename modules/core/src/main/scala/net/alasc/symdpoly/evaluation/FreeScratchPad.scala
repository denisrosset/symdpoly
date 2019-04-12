package net.alasc.symdpoly.evaluation

import java.util.WeakHashMap
import java.util.Collections

import scala.annotation.tailrec

import shapeless.Witness
import spire.syntax.cfor._

import metal.IsVPtr
import metal.syntax._

import net.alasc.symdpoly.free.MutableWord
import net.alasc.symdpoly.math.Phase
import net.alasc.symdpoly.{free, valueOf}

/** Scratch pad of mutable words where equivalence operations are applied in batch.
  * The pad itself is an array of words, all stored in their phase canonical form. Additionally, the map "phases"
  * stores the corresponding phase. This enables to quickly check whether a monomial is equivalent to zero.
  *
  * At any point, the first "n" words are part of the equivalence class. Usually, we start by setting n = 1, and having
  * the first word corresponding to the monomial whose canonical representative we want to compute. These first "n" words
  * are locked: their content is not mutated anymore, and their hash value is cached.
  *
  * The first "n" words are also entered in the HashMap "phases", which stores their phase relative to the canonical
  * phase representative stored in the scratch pad.
  *
  * There is no way to store a zero in the scratch pad.
  */
class FreeScratchPad[F <: free.MonoidDef with Singleton: Witness.Aux](var pad: Array[MutableWord[F]], var n: Int, var phaseArray: Array[Int], val phaseMap: metal.mutable.HashMap[MutableWord[F], Int]) { self =>

  /** Verifies class invariants, used for debugging purposes. */
  def checkInvariants(): Unit = {
    assert(phaseMap.size == n)
    assert(n <= pad.length)
    cforRange(0 until n) { i =>
      val w = pad(i)
      assert(w.state == MutableWord.Locked)
      assert(w.phase == Phase.one)
      assert(w.length >= 0)
      assert(phaseMap.contains(w))
      assert(phaseArray(i) == phaseMap(w))
    }
    cforRange(n until pad.size) { i =>
      val w = pad(i)
      assert(w.state == MutableWord.Mutable)
    }
  }

  def F: F = valueOf[F]

  override def toString: String = {
    val first = (0 until n).map(i => i.toString.padTo(4, ' ') + "* " + pad(i).toString + " " + System.identityHashCode(pad(i)).toString)
    val second = (n until pad.length).map(i => i.toString.padTo(4, ' ') + "  " + pad(i).toString + " " + System.identityHashCode(pad(i)).toString)
    (first ++ second).mkString("\n")
  }

  // Scratchpad manipulation (resize, clear)

  /** Clears the scratch pad and sets its first element to the given word. */
  def resetWithCopyOf(mono: MutableWord[F]): FreeScratchPad[F] = {
    require(!mono.isZero)
    cforRange(0 until pad.length) { i =>
      pad(i).reset()
    }
    phaseMap.reset()
    scratch(0).setToContentOf(mono)
    scratch(0).setPhase(Phase.one)
    scratch(0).lock()
    phaseMap(scratch(0)) = mono.phase.encoding
    phaseArray(0) = mono.phase.encoding
    n = 1
    self
  }

  /** Returns the scratch element at the i-th position, growing the pad size if necessary. */
  def scratch(i: Int): MutableWord[F] = {
    if (i >= pad.length) ensureMinimalSize(spire.math.max(i + 1, 2 * pad.length))
    assert(pad(i) ne null)
    pad(i)
  }

  /** Sets the pad size. */
  def setSize(newSize: Int): Unit = {
    require(newSize >= n)
    if (newSize != n) {
      val newScratch = new Array[free.MutableWord[F]](newSize)
      System.arraycopy(pad, 0, newScratch, 0, pad.length)
      val newPhaseArray = new Array[Int](newSize)
      System.arraycopy(phaseArray, 0, newPhaseArray, 0, phaseArray.length)
      val reservedSize = if (n == 0) 8 else newScratch(0).reservedSize
      cforRange(pad.length until newScratch.length) { i =>
        newScratch(i) = free.MutableWord.one[F](reservedSize)
      }
      pad = newScratch
      phaseArray = newPhaseArray
    }
  }

  /** Ensures that the pad has the given minimal size, growing it if necessary, but never shrinking it. */
  def ensureMinimalSize(minimalSize: Int): Unit = setSize(spire.math.max(minimalSize, pad.length))

  /** Registers the words at position n until newN.
    *
    * Returns true if a phase mismatch occurred, i.e. the word is equivalent to zero.
    */
  def registerAndTestForZero(newN: Int): Boolean = {
    // recurse with three parts
    // (n until cur) contains new words which have been locked and registered in "phases"
    // (cur until bound) contains words which are still mutable and unchecked
    // (bound until newN) contains words which were found to be duplicates and were reset to be reused (mutable)
    //
    // Returns either bound (word is not equivalent to zero), or ~bound (word is equivalent to zero)
    @tailrec def rec(cur: Int, bound: Int): Int =
      if (cur == bound) bound else {
        val w = pad(cur)
        val phase = w.phase
        w.setPhase(Phase.one)
        w.lock()
        phaseMap.ptrFind(w) match {
          case IsVPtr(vp) => // word is already known
            val storedPhase: Int = vp.value
            w.reset() // in any case we reset the word
            // check if phases match
            if (storedPhase == phase.encoding) {
              // if yes, the word is redundant
              if (cur != bound - 1) {
                val tmp = pad(bound - 1)
                pad(bound - 1) = w
                pad(cur) = tmp
              }
              rec(cur, bound - 1)
            } else ~cur // if not, the word is equivalent to zero
          case _ =>
            phaseMap(w) = phase.encoding
            phaseArray(cur) = phase.encoding
            rec(cur + 1, bound)
        }
      }
    val res = rec(n, newN)
    if (res >= 0) {
      n = res
      false
    } else {
      n = ~res
      true
    }
  }

}

object FreeScratchPad {

  private val map = new WeakHashMap[free.MonoidDef, FreeScratchPad[_ <: free.MonoidDef with Singleton]]

  def release[F <: free.MonoidDef with Singleton](pad: FreeScratchPad[F]): Unit =
    synchronized { map.put(pad.F, pad) }

  def apply[F <: free.MonoidDef with Singleton:Witness.Aux]: FreeScratchPad[F] = synchronized {
    Option(map.remove(valueOf[F])) match {
      case Some(pad) => pad.asInstanceOf[FreeScratchPad[F]]
        create[F]
      case None => create[F]
    }
  }

  protected def create[F <: free.MonoidDef with Singleton:Witness.Aux]: FreeScratchPad[F] =
    new FreeScratchPad[F](Array.empty[free.MutableWord[F]], 0, Array.empty[Int], metal.mutable.HashMap.empty[MutableWord[F], Int])

}
