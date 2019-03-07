package net.alasc.symdpoly
package syntax

import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.math.Phase
import spire.macros.Ops

/** Syntax enrichment for phased. */
final class PhasedOps[A](lhs: A)(implicit ev: Phased[A]) {
  def phaseOffset(): Phase = macro Ops.unop[Phase]
  def phaseCanonical(): A = macro Ops.unop[A]
}
