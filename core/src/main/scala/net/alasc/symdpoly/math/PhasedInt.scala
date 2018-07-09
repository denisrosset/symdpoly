package net.alasc.symdpoly.math

import org.scalacheck.{Arbitrary, Gen}
import spire.algebra.Eq

import net.alasc.symdpoly.Phase

/** Described a domain element (integer >= 0) along with a phase. */
class PhasedInt(val encoding: Long) extends AnyVal {
  override def toString: String = {
    val phasePrefix = if (phase.isOne) ""
    else if (phase.isMinusOne) "-"
    else phase.toString + "*"
    s"${phasePrefix}x${index}"
  }
  def get: PhasedInt = this
  def _1: Phase = phase
  def _2: Int = index
  def isEmpty: Boolean = false
  def index: Int = encoding.toInt
  def phase: Phase = new Phase((encoding >>> 32).toInt)
}

object PhasedInt {
  /** Name-based extractor enabling the syntax
    * val PhasedInt(index, phase) = phasedInt
    */
  def unapply(pi: PhasedInt): PhasedInt = pi
  def apply(phase: Phase, index: Int): PhasedInt = new PhasedInt((phase.encoding.toLong << 32) + index.toLong)
  def gen: Gen[PhasedInt] = for {
    index <- Gen.choose(0, 100)
    phase <- Phase.gen
  } yield PhasedInt(phase, index)
  implicit val arb: Arbitrary[PhasedInt] = Arbitrary(gen)
  implicit val equ: Eq[PhasedInt] = Eq.fromUniversalEquals
}
