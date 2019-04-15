package net.alasc.symdpoly

//import org.scalacheck.Shrink
//import org.scalacheck.Test.Parameters
//import org.scalactic.{Prettifier, source}
//import org.scalatest.enablers.UnitCheckerAsserting
//import org.scalatest.exceptions.{GeneratorDrivenPropertyCheckFailedException, StackDepthException}
//import org.scalatest.{Assertion, Succeeded}
import org.typelevel.discipline.Laws
//import shapeless.Witness
import spire.algebra._
import spire.syntax.all._
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object MorphismLaws {
  def apply[A:Eq:Arbitrary, B:Eq](morphism0: A => B): MorphismLaws[A, B] = new MorphismLaws[A, B] {
    def morphism: A => B = morphism0
    def ArbA: Arbitrary[A] = implicitly[Arbitrary[A]]
    def EqA: Eq[A] = implicitly[Eq[A]]
    def EqB: Eq[B] = implicitly[Eq[B]]
  }
}

trait MorphismLaws[A, B] extends Laws {

  implicit def ArbA: Arbitrary[A]
  implicit def EqA: Eq[A]
  implicit def EqB: Eq[B]
  def morphism: A => B

  def multiplicativeSemigroup(implicit A: MultiplicativeSemigroup[A], B: MultiplicativeSemigroup[B]) = new DefaultRuleSet(
    "morphism[multiplicativesemigroup]",
    parent = None,
    "preserves product" -> forAll( (x: A, y: A) =>
      morphism(x) * morphism(y) === morphism(x * y)
    )
  )

  def multiplicativeMonoid(implicit A: MultiplicativeMonoid[A], B: MultiplicativeMonoid[B]) = new DefaultRuleSet(
    name = "morphism[multiplicativemonoid]",
    parent = Some(multiplicativeSemigroup),
    "preserves one" -> Prop( morphism(A.one) === B.one )
  )
}
