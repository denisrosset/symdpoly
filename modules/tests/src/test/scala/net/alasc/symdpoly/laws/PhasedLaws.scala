package net.alasc.symdpoly
package laws

import spire.algebra.{Eq, Involution, MultiplicativeAbGroup, MultiplicativeAction}
import spire.laws.{ActionLaws, GroupLaws}
import spire.syntax.eq._
import spire.syntax.action._
import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.math.Phase
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Predicate
import spire.laws.InvalidTestException.forAllSafe

object PhasedLaws {

  def apply[A:Eq:Arbitrary](implicit groupLaws0: GroupLaws[Phase]): PhasedLaws[A] = new PhasedLaws[A] {
    val scalarLaws: GroupLaws[Phase] = groupLaws0
    def EquA = Eq[A]
    def ArbA = implicitly[Arbitrary[A]]
  }

}

/** Laws for elements with a phase. */
trait PhasedLaws[A] extends ActionLaws[Phase, A] {

  import scalarLaws.Arb

  implicit def EquA: Eq[A]
  implicit def ArbA: Arbitrary[A]

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

  def phasedInvolution(implicit A: Phased[A], G: MultiplicativeAction[A, Phase], I: Involution[A]) = new MultiplicativeProperties(
    base = groupAction(G.multiplicative, MultiplicativeAbGroup[Phase].multiplicative),
    parent = Some(phased),
    "adjoint and phaseCanonical commute" -> forAllSafe { (a: A) =>
      (A.phaseCanonical(I.adjoint(a)) === I.adjoint(A.phaseCanonical(a)))
    }
  )

}
