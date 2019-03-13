package net.alasc.symdpoly
package algebra


import cats.Invariant
import net.alasc.symdpoly.math.Phase
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
  * When Involution[A] exists, we must have a.adjoint.phaseCanonical === a.phaseCanonical.adjoint.
  *
  */
trait Phased[A] extends MultiplicativeAction[A, Phase] {
  /** Canonical element in the equivalence class of elements under phase action. */
  def phaseCanonical(a: A): A
  /** Phase offset of an element with respect to its canonical representative. */
  def phaseOffset(a: A): Phase
}

object Phased {

  def apply[A](implicit ev: Phased[A]): Phased[A] = ev

  /** Describes how [[Phased]] transforms under isomorphism. */
  implicit val invariant: Invariant[Phased] = new Invariant[Phased] {
    def imap[A, B](fa: Phased[A])(f: A => B)(g: B => A): Phased[B] = new Phased[B] {
      def phaseOffset(b: B): Phase = fa.phaseOffset(g(b))
      def phaseCanonical(b: B): B = f(fa.phaseCanonical(g(b)))
      def gtimesl(phase: Phase, b: B): B = f(fa.gtimesl(phase, g(b)))
      def gtimesr(b: B, phase: Phase): B = f(fa.gtimesr(g(b), phase))
    }
  }

}
