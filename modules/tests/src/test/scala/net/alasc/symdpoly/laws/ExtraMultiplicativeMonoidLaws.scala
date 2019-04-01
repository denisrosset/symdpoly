package net.alasc.symdpoly
package laws

import spire.algebra.Eq
import spire.laws.InvalidTestException.forAllSafe
import spire.syntax.eq._

import net.alasc.symdpoly.algebra.MultiplicativeBinoid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.typelevel.discipline.{Laws, Predicate}

object ExtraMultiplicativeMonoidLaws {
  def apply[A:Eq:Arbitrary](implicit _pred: Predicate[A]): ExtraMultiplicativeMonoidLaws[A] = new ExtraMultiplicativeMonoidLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
    def pred = _pred
    def nonZeroArb = Arbitrary(arbitrary[A] filter _pred)
  }
}

/** Contains additional laws for multiplicative monoids with extra structure. */
trait ExtraMultiplicativeMonoidLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]
  def pred: Predicate[A]
  def nonZeroArb: Arbitrary[A]

  def multiplicativeBinoid(implicit A: MultiplicativeBinoid[A]) = new DefaultRuleSet(
    name = "multiplicativeBinoid",
    parent = None,
    "left absorption" -> forAllSafe((a: A) => A.times(A.zero, a) === A.zero),
    "right absorption" -> forAllSafe((a: A) => A.times(a, A.zero) === A.zero),
    "zero product" -> (A.times(A.zero, A.zero) === A.zero)
  )

}