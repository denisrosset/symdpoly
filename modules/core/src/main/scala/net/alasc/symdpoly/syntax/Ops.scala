package net.alasc.symdpoly
package syntax

import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.math.Phase
import spire.algebra.{Eq, Involution, VectorSpace}
import spire.macros.Ops
import spire.syntax.vectorSpace._
import spire.syntax.involution._

/** Syntax enrichment for phased. */
final class PhasedOps[A](lhs: A)(implicit ev: Phased[A]) {
  def phaseOffset(): Phase = macro Ops.unop[Phase]
  def phaseCanonical(): A = macro Ops.unop[A]
}
