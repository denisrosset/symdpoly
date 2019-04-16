package net.alasc.symdpoly
package syntax

import net.alasc.symdpoly.algebra.Phased
import spire.algebra.Involution

trait PhasedSyntax {
  implicit def phasedOps[A: Phased](a: A): PhasedOps[A] = new PhasedOps(a)
}

/** Syntax for pretty printing objects */
trait PrinterSyntax {
  implicit def prettyOps[A](a: A): PrinterOps[A] = new PrinterOps[A](a)
}

trait AllSyntax extends
  PhasedSyntax with
  PrinterSyntax
