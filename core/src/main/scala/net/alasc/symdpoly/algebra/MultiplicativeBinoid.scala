package net.alasc.symdpoly.algebra

import cats.kernel.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.typelevel.discipline.{Laws, Predicate}
import spire.algebra.MultiplicativeMonoid
import spire.laws.InvalidTestException.forAllSafe
import spire.syntax.eq._

trait MultiplicativeBinoid[A] extends MultiplicativeMonoid[A] {
  def zero: A
  def isZero(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, zero)
}

object ExtraMultiplicativeMonoidLaws {
  def apply[A:Eq:Arbitrary](implicit _pred: Predicate[A]) = new ExtraMultiplicativeMonoidLaws[A] {
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
