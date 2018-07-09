package net.alasc.symdpoly.algebra

import cats.kernel.Eq

import org.scalacheck.Arbitrary
import org.typelevel.discipline.Predicate
import spire.algebra.{MultiplicativeAbGroup, MultiplicativeAction}
import spire.laws.ActionLaws
import spire.laws.InvalidTestException.forAllSafe
import spire.syntax.action._
import spire.syntax.eq._
import spire.macros.Ops

import net.alasc.symdpoly.Phase

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

/** Laws for elements with a phase. */
trait PhasedLaws[A] extends ActionLaws[Phase, A] {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]
  def pred: Predicate[A]
  def nonZeroArb: Arbitrary[A]

  def multiplicativeGroupAction(implicit G: MultiplicativeAction[A, Phase]) = new MultiplicativeProperties(
    base = groupAction(G.multiplicative, MultiplicativeAbGroup[Phase].multiplicative),
    parent = None
  )

  def phased(implicit A: Phased[A], G: MultiplicativeAction[A, Phase]) = new MultiplicativeProperties(
    base = groupAction(G.multiplicative, MultiplicativeAbGroup[Phase].multiplicative),
    parent = Some(multiplicativeGroupAction),
    "phaseCanonical(a) <* phaseOffset(a) === a" -> forAllSafe { (a: A) =>
      (A.phaseCanonical(a) <* A.phaseOffset(a)) === a
    },
    "phaseCanonical(a) === phaseCanonical(a <* phase)" -> forAllSafe { (a: A, phase: Phase) =>
      A.phaseCanonical(a) === A.phaseCanonical(a <* phase)
    }
  )

}
