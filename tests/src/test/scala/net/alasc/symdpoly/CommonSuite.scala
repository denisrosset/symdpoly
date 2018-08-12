package net.alasc.symdpoly

import cyclo.RealCyclo
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.FunSuite
import shapeless.Witness
import spire.algebra.{CRing, MultiplicativeMonoid, Ring}
import spire.syntax.ring._
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.Predicate
import org.typelevel.discipline.scalatest.Discipline
import scala.collection.immutable.BitSet

/*
object RandomElements {



  def genBitSet(size: Int): Gen[BitSet] =
    Gen.listOfN(size, Gen.oneOf(true,false)).map(l => BitSet(l.zipWithIndex.filter(_._1).map(_._2): _*))

  def genMonForSize(size: Int): Gen[Mon] = for {
    p <- net.alasc.laws.Permutations.permForSize(size)
    f <- genBitSet(size)
  } yield Mon(p, f)

  def genMon: Gen[Mon] = Gen.parameterized(params => genMonForSize(params.size))

  implicit def arbMon: Arbitrary[Mon] = Arbitrary(genMon)
}


class FreeAlgebraSuite extends CommonSuite {
  import cyclo.Cyclos.{arbCyclo, arbRealCyclo, NonZero, RealNonZero}
  import RandomElements.{arbOb, arbFreeMono, arbFreePoly}
  object algebra extends FreeAlgebra[RealCyclo] {
    val A = hermitian(2)
    val B = hermitian(2)

  }

  checkAll("Free algebra monomial monoid", spire.laws.RingLaws[Mono[algebra.type]].multiplicativeMonoid)
  checkAll("Free algebra polynomial ring", spire.laws.RingLaws[Poly[algebra.type, RealCyclo]].ring)
}

class MonSuite extends CommonSuite {
  import RandomElements.arbMon
  checkAll("Mon", spire.laws.GroupLaws[Mon].group)
  checkAll("Mon", net.alasc.laws.PermutationActionLaws[Mon].faithfulPermutationAction(implicitly, Mon.NaturalPermutationAction))
  checkAll("Mon", spire.laws.ActionLaws[Mon, Int].groupAction(Mon.SignIntAction, implicitly))
}

class GroupWithGeneratorsSuite extends CommonSuite {
  import net.alasc.finite._
  import net.alasc.perms._
  import net.alasc.perms.default._
  val G = GroupWithGenerators("t" -> Perm(0, 1), "c" -> Perm(0,1,2,3))
  val Seq(t, c) = G.generatorWords
  val grp: Grp[Word[G.type, Perm]] = Grp(t, c)


  implicit def arbWord[H <: GroupWithGenerators[A] with Singleton:Witness.Aux, A]: Arbitrary[Word[H, A]] = Arbitrary { genWord[H, A](2) }

  checkAll("Word", spire.laws.GroupLaws[Word[G.type, Perm]].group)
  checkAll("Word", net.alasc.laws.PermutationActionLaws[Word[G.type, Perm]].faithfulPermutationAction(implicitly, G.wordTypeclasses))
}

class CHSHAlgebraSuite extends CommonSuite {
  import cyclo.Cyclos.{arbCyclo, arbRealCyclo, NonZero, RealNonZero}
  import RandomElements.{arbOb, arbFreeMono, arbFreePoly, arbQuotientMono}

  object free extends FreeAlgebra[RealCyclo] {
    val A = hermitian(2)
    val B = hermitian(2)
  }

  import free.{A, B}

  val quotient = FastQuotientRing(free) {
    case (A(_), B(_)) => FastSubs.Keep
    case (B(_), A(_)) => FastSubs.Swap
    case (A(x1), A(x2)) =>
      if (x1 == x2) FastSubs.Discard else FastSubs.Keep
    case (B(y1), B(y2)) =>
      if (y1 == y2) FastSubs.Discard else FastSubs.Keep
    case _ => FastSubs.Keep
  }

  //implicit def arbMono: Arbitrary
  checkAll("Quotient algebra monomial monoid", spire.laws.RingLaws[Mono[quotient.type]](implicitly, arbQuotientMono[quotient.type, free.type, RealCyclo], implicitly).multiplicativeMonoid)
  //checkAll("Free algebra polynomial ring", spire.laws.RingLaws[Poly[quotient.type, RealCyclo]].ring)
}

*/
/**
  * An opinionated stack of traits to improve consistency and reduce
  * boilerplate.
  */
trait CommonSuite extends FunSuite with Matchers
  with PropertyChecks
  with Discipline
  with spire.syntax.AllSyntax with spire.std.AnyInstances {

  def noShrink[T] = Shrink[T](_ => Stream.empty)

}
