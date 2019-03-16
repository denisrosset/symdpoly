package net.alasc.symdpoly
package syntax

import net.alasc.symdpoly.algebra.Phased
import spire.algebra.Involution

trait PhasedSyntax {
  implicit def phasedOps[A: Phased](a: A): PhasedOps[A] = new PhasedOps(a)
}

trait RichInvolutionSyntax {
  implicit def richInvolutionOps[A: Involution](a: A): RichInvolutionOps[A] = new RichInvolutionOps(a)
}

trait AllSyntax extends
  PhasedSyntax with
  RichInvolutionSyntax
