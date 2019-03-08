package net.alasc.symdpoly

import cyclo.Cyclo
import org.typelevel.discipline.Predicate
import spire.laws.{InvolutionLaws, RingLaws}
import defaults._
import net.alasc.symdpoly.laws.ExtraMultiplicativeMonoidLaws
import spire.math.Rational
import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.symdpoly.generic.{FreeBasedMono, FreeBasedPoly}
import net.alasc.symdpoly.math.Phase

class CHSHAlgebraSuite extends CommonSuite {

  import laws.Monos._
  import laws.Polys._

  object Free extends free.MonoidDef(2) {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianOpFamily1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianOpFamily1(0 to 1)

    val operators = Seq(A, B)
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

  val CHSH = A(0)*B(0) + B(1)*A(0) + A(1)*B(0) - A(1)*B(1)

  checkAll("free monoid", RingLaws[FreeBasedMono.Free[Free.type]].multiplicativeMonoid)
  checkAll("free monoid involution", InvolutionLaws[FreeBasedMono.Free[Free.type]].involutionMultiplicativeMonoid)
  checkAll("free binoid", ExtraMultiplicativeMonoidLaws[FreeBasedMono.Free[Free.type]].multiplicativeBinoid)

  val Quotient = Free.quotientMonoid(quotient.pairs {
    case (A(x1), A(x2)) if x1 == x2 => FreeBasedMono.one
    case (B(y1), B(y2)) if y1 == y2 => FreeBasedMono.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  })

  val ambientGroup = Quotient.groupInQuotient(Grp(swapParties, swapInputA, swapOutputA0))

  val gset: GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A, B))

  val L: Evaluator[Quotient.type] = Quotient.evaluator(evaluation.real)
  val gramMatrix = GramMatrix[L.type, Quotient.type](L, gset)

  checkAll("quotient monoid", RingLaws[FreeBasedMono[Quotient.type, Free.type]].multiplicativeMonoid)
  checkAll("quotient monoid involution", InvolutionLaws[FreeBasedMono[Quotient.type, Free.type]].involutionMultiplicativeMonoid)
  checkAll("quotient binoid", ExtraMultiplicativeMonoidLaws[FreeBasedMono[Quotient.type, Free.type]].multiplicativeBinoid)

  import cyclo.Cyclos.arbCyclo
  implicit val cycloPred: Predicate[Cyclo] = { (c: Cyclo) => !c.isZero}
  checkAll("nc poly ring", spire.laws.RingLaws[FreeBasedPoly[Quotient.type, Free.type]].ring)
  checkAll("nc ring as vector space", spire.laws.VectorSpaceLaws[FreeBasedPoly[Quotient.type, Free.type], Cyclo].vectorSpace)
  checkAll("nc ring involution", spire.laws.InvolutionLaws[FreeBasedPoly[Quotient.type, Free.type]].involutionMultiplicativeMonoid)

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
