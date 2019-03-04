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

  /** Index of the domain encoded. */
  def index: Int = encoding.toInt

  /** Phase. */
  def phase: Phase = new Phase((encoding >>> 32).toInt)

  /** Method used for named-based extractor syntax. */
  def isEmpty: Boolean = false
  /** Method used for named-based extractor syntax. */
  def get: PhasedInt = this
  /** Returns the first element of this [[PhasedInt]], which is the phase. */
  def _1: Phase = phase
  /** Returns the second element of this [[PhasedInt]] which is an integer from the domain. */
  def _2: Int = index

}

object PhasedInt {

  /** Name-based extractor enabling the syntax
    * val PhasedInt(index, phase) = phasedInt
    */
  def unapply(pi: PhasedInt): PhasedInt = pi

  /** Constructs a [[PhasedInt]] from a phase and an integer. */
  def apply(phase: Phase, index: Int): PhasedInt = new PhasedInt((phase.encoding.toLong << 32) + index.toLong)

  /** Scalacheck generator of a random [[PhasedInt]]. */
  def gen: Gen[PhasedInt] = for {
    index <- Gen.choose(0, 100)
    phase <- Phase.gen
  } yield PhasedInt(phase, index)

  /** Arbitrary [[PhasedInt]] used in property tests. */
  implicit val arb: Arbitrary[PhasedInt] = Arbitrary(gen)

  /** Equality typeclass instance for [[PhasedInt]]. */
  implicit val equ: Eq[PhasedInt] = Eq.fromUniversalEquals

}
