package net.alasc.symdpoly
package syntax

import net.alasc.symdpoly.algebra.Phased
import spire.algebra.Involution

trait PhasedSyntax {
  implicit def phasedOps[A: Phased](a: A): PhasedOps[A] = new PhasedOps(a)
}

trait AllSyntax extends
  PhasedSyntax
