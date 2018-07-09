package net.alasc.symdpoly

import org.scalacheck.Shrink
import org.scalacheck.Test.Parameters
import org.scalactic.{Prettifier, source}
import org.scalatest.enablers.UnitCheckerAsserting
import org.scalatest.exceptions.{GeneratorDrivenPropertyCheckFailedException, StackDepthException}
import org.scalatest.{Assertion, Succeeded}
import org.typelevel.discipline.Laws
import shapeless.Witness
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
/*
trait Checks extends UnitCheckerAsserting {

  def checkAll(name: String, ruleSet: Laws#RuleSet)(implicit position: org.scalactic.source.Position): Unit = {
    val ca = new CheckerAssertingImpl[Assertion] {
      type Result = Assertion
      override def indicateSuccess(message: => String): Assertion = {
        println(message)
        Succeeded
      }
      override def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, scalaCheckArgs: List[Any], scalaCheckLabels: List[String], optionalCause: Option[Throwable], pos: source.Position): Assertion = {
        throw new GeneratorDrivenPropertyCheckFailedException(
          messageFun,
          optionalCause,
          pos,
          None,
          undecoratedMessage,
          scalaCheckArgs,
          None,
          scalaCheckLabels.toList
        )
      }
    }
    for ((id, prop) ← ruleSet.all.properties) {
      print(s"${name}.${id} ")
      ca.check(prop, Parameters.default, Prettifier.default, position)
    }
  }

  def checkMonoid[M <: GenMonoid with Singleton]
  (implicit w: Witness.Aux[M], arb: Arbitrary[OldMono[M]], mm: MultiplicativeMonoid[OldMono[M]], i: Involution[OldMono[M]]): Unit = {
    def M: M = w.value
    checkAll(M.name, spire.laws.RingLaws[OldMono[M]].multiplicativeMonoid)
    checkAll(M.name, spire.laws.InvolutionLaws[OldMono[M]].involutionMultiplicativeMonoid)
  }

  def checkFastQuotientMonoid[Q <: FastQuotientMonoid with Singleton]
  (implicit wQ: Witness.Aux[Q], arb: Arbitrary[OldMono[Q]], mm: MultiplicativeMonoid[OldMono[Q]], i: Involution[OldMono[Q]]): Unit = {
    def Q: Q = wQ.value
    implicit def wF: Witness.Aux[Q#F] = Q.witnessF
    checkAll(Q.F.name, spire.laws.RingLaws[OldMono[Q#F]].multiplicativeMonoid)
    checkAll(Q.F.name, spire.laws.InvolutionLaws[OldMono[Q#F]].involutionMultiplicativeMonoid)
    checkAll(Q.name, MorphismLaws[OldMono[Q#F], OldMono[Q]]((mono: OldMono[Q#F]) => Q.apply(mono) ).multiplicativeMonoid)
    checkAll(Q.name, spire.laws.RingLaws[OldMono[Q]].multiplicativeMonoid)
    checkAll(Q.name, spire.laws.InvolutionLaws[OldMono[Q]].involutionMultiplicativeMonoid)
  }

  class CheckAmbientGroupOps[F <: FreeMonoid with Singleton](val ambientGroup: GroupWithGenerators[Generator[F]]) {
    def onMonoid[M <: FastQuotientMonoid.Aux[F] with Singleton](implicit w: Witness.Aux[M], arb: Arbitrary[SignedMono[M]]): Unit = {
      def M: M = w.value
      implicit def arbW: Arbitrary[Word[ambientGroup.type, Generator[F]]] = Word.arbWord[ambientGroup.type, Generator[F]]
      checkAll(ambientGroup.name + " on " + M.name,  spire.laws.ActionLaws[Word[ambientGroup.type, Generator[F]], SignedMono[M]].groupAction)
    }
  }

  def checkAmbientGroup[F <: FreeMonoid with Singleton](ambientGroup: GroupWithGenerators[Generator[F]]): CheckAmbientGroupOps[F]
  = new CheckAmbientGroupOps[F](ambientGroup)

  /*def checkPolyOnMonoid[M <: GenMonoid with Singleton, K:Arbitrary:Eq:Field:Involution]
  (implicit w: Witness.Aux[M], equ: Eq[Poly[M, K]], arb: Arbitrary[Mono[M]], mm: MultiplicativeMonoid[Mono[M]], i: Involution[Mono[M]]): Unit = {
    def M: M = w.value
    checkAll(M.name, spire.laws.RingLaws[Poly[M, K]].ring)
    checkAll(M.name, spire.laws.VectorSpaceLaws[Poly[M, K], K].vectorSpace)
    checkAll(M.name, spire.laws.InvolutionLaws[Poly[M, K]].involutionMultiplicativeMonoid)
  }*/

  def noShrink[T]: Shrink[T] = Shrink[T](_ => Stream.empty)

}
*/
