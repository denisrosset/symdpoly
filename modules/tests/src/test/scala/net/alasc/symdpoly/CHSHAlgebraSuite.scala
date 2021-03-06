package net.alasc.symdpoly

import cyclo.Cyclo
import org.typelevel.discipline.Predicate
import spire.laws.{GroupLaws, InvolutionLaws, RingLaws}
import defaults._
import net.alasc.symdpoly.laws.{ExtraMultiplicativeMonoidLaws, PhasedLaws}
import spire.math.Rational
import net.alasc.symdpoly.freebased.{Mono, Poly}
import net.alasc.symdpoly.math.Phase
import org.scalacheck.Arbitrary

class CHSHAlgebraSuite extends CommonSuite {

  import laws.Monos._
  import laws.Polys._

  object Free extends free.MonoDef(2) {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianOpFamily1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianOpFamily1(0 to 1)

    lazy val families = Seq(A, B)
  }

  import Free.{A, B}

  val swapParties = Free.permutation {
    case A(i) => B(i)
    case B(i) => A(i)
  }

  val swapInputA = Free.permutation {
    case A(x) => A(1-x)
    case B(y) => B(y)
  }

  val swapOutputA0 = Free.permutation {
    case A(0) => -A(0)
    case op => op
  }

  checkAll("free monoid", RingLaws[Free.MonoType].multiplicativeMonoid)
  checkAll("free monoid involution", InvolutionLaws[Free.MonoType].involutionMultiplicativeMonoid)
  checkAll("free binoid", ExtraMultiplicativeMonoidLaws[Free.MonoType].multiplicativeBinoid)

  val Quotient = Free.quotientMonoid(quotient.pairs {
    case (A(x1), A(x2)) if x1 == x2 => Mono.one
    case (B(y1), B(y2)) if y1 == y2 => Mono.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  })

  val CHSH = Quotient.quotient(A(0)*B(0) + B(1)*A(0) + A(1)*B(0) - A(1)*B(1))

  val ambientGroup = Quotient.groupInQuotient(Grp(swapParties, swapInputA, swapOutputA0))
  val symmetryGroup = CHSH.invariantSubgroupOf(ambientGroup)

  val gset: GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A, B))

  val L = Quotient.eigenvalueEvaluator(true)
  val Lsym = L.symmetrize(symmetryGroup)

  checkAll("quotient monoid", RingLaws[Mono[Quotient.type, Free.type]].multiplicativeMonoid)
  checkAll("quotient monoid involution", InvolutionLaws[Mono[Quotient.type, Free.type]].involutionMultiplicativeMonoid)
  checkAll("quotient binoid", ExtraMultiplicativeMonoidLaws[Mono[Quotient.type, Free.type]].multiplicativeBinoid)
  val groupLaws = GroupLaws[Phase](implicitly, Arbitrary(Phase.genForDenominator(Free.cyclotomicOrder)))
  checkAll("quotient phased", PhasedLaws[Mono[Quotient.type, Free.type]](implicitly, implicitly, groupLaws).phasedInvolution)

  import cyclo.Cyclos.arbCyclo
  implicit val cycloPred: Predicate[Cyclo] = { (c: Cyclo) => !c.isZero}
  checkAll("nc poly ring", spire.laws.RingLaws[Poly[Quotient.type, Free.type]].ring)
  checkAll("nc ring as vector space", spire.laws.VectorSpaceLaws[Poly[Quotient.type, Free.type], Cyclo].vectorSpace)
  checkAll("nc ring involution", spire.laws.InvolutionLaws[Poly[Quotient.type, Free.type]].involutionMultiplicativeMonoid)

  test("Optimized vs unoptimized quotient") {
    forAll { (m: Quotient.MonoType) =>
      optimized(L(m)) === unoptimized(L(m))
    }
    forAll { (m: Quotient.MonoType) =>
      optimized(Lsym(m)) === unoptimized(Lsym(m))
    }
  }

  test("Partially commutative parts") {
    val pc = evaluation.parts.PartiallyCommutative(Quotient)
    assert(pc.partition.fold(sys.error, partition => partition.underlying.nBlocks) == 2)
  }

  test("Sign interpretation") {
    val e1 = A(1) - A(0) - A(0)
    val e2 = - A(0) + A(1) - A(0)
    val e3 = - A(0) - A(0) + A(1)
    val e4 = - A(0) - A(0) - A(1)
    val e5 = - (A(0) + A(0)) - A(1)
    assert(e1 === e2)
    assert(e1 === e3)
    assert(e1 =!= e4)
    assert(e4 === e5)
  }

  test("Syntax") {
    val op = A(0)
    val phasedOp = -A(0)
    val mono = A(0)*A(1)
    val phase1 = Phase.one
    val int1 = 1
    val rat1 = Rational.one
    val cyc1 = Cyclo.one

    // Op unary_-
    -op
    // Op +/-
    op + op
    op + phasedOp
    op + mono
    op + phase1
    op + int1
    op + rat1
    op + cyc1
    op - op
    op - phasedOp
    op - mono
    op - phase1
    op - int1
    op - rat1
    op - cyc1
    // Op *
    op * phase1
    op * int1
    op * rat1
    op * cyc1
  }

}
