package net.alasc.symdpoly
package syntax

import net.alasc.symdpoly.algebra.Phased

trait PhasedSyntax {
  implicit def phasedOps[A: Phased](a: A): PhasedOps[A] = new PhasedOps(a)
}

trait AllSyntax extends
  PhasedSyntax
