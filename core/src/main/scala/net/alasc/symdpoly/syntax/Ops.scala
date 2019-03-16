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

final class RichInvolutionOps[A](lhs: A)(implicit ev: Involution[A]) {
  def isSelfAdjoint(implicit ev1: Eq[A]): Boolean = ev1.eqv(lhs, ev.adjoint(lhs))
  def realPart[F](implicit ev1: VectorSpace[A, F]): A = (lhs + lhs.adjoint) :* ev1.scalar.fromInt(2).reciprocal
}
