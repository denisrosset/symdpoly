package net.alasc.symdpoly
package algebra


import spire.algebra.MultiplicativeAction
import spire.macros.Ops

/** Describes an element with a phase.
  * Each element a \in A is associated to an equivalence class (up to phase) written
  * [a] = {a <* phase | phase \in AllPhases }.
  *
  * Then phaseCanonical(a) returns a canonical representative of the equivalence class [a],
  * and phaseOffset(a) returns the phase of a relative to phaseCanonical(a) such that
  *
  * a === phaseCanonical(a) <* phaseOffset(a)
  *
  */
trait Phased[A] extends MultiplicativeAction[A, Phase] {
  def phaseOffset(a: A): Phase
  def phaseCanonical(a: A): A
}

object Phased {
  def apply[A](implicit ev: Phased[A]): Phased[A] = ev
  final class PhasedOps[A](lhs: A)(implicit ev: Phased[A]) {
    def phaseOffset(): Phase = macro Ops.unop[Phase]
    def phaseCanonical(): A = macro Ops.unop[A]
  }
  object syntax {
    implicit def phasedOps[A:Phased](a: A): PhasedOps[A] = new PhasedOps(a)
  }
}
