package net.alasc.symdpoly
package laws

import spire.algebra.{Eq, MultiplicativeAbGroup, MultiplicativeAction}
import spire.laws.ActionLaws
import spire.syntax.eq._
import spire.syntax.action._
import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.math.Phase
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Predicate
import spire.laws.InvalidTestException.forAllSafe

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
