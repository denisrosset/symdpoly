package net.alasc.symdpoly

import cyclo.Cyclo

import org.typelevel.discipline.Predicate
import spire.laws.{InvolutionLaws, RingLaws}

import defaults._
import net.alasc.symdpoly.laws.ExtraMultiplicativeMonoidLaws
import spire.math.Rational

import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.symdpoly.generic.FreeBasedMono

class CHSHAlgebraSuite extends CommonSuite {

  import laws.Monos._
  import laws.Polys._

  object FM extends free.MonoidDef(2) {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianOpType1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianOpType1(0 to 1)

    val operators = Seq(A, B)
  }

  import FM.{A, B}

  val swapParties = FM.permutation {
    case A(i) => B(i)
    case B(i) => A(i)
  }

  val swapInputA = FM.permutation {
    case A(x) => A(1-x)
    case B(y) => B(y)
  }

  val swapOutputA0 = FM.permutation {
    case A(0) => A(0)*Phase.minusOne
    case op => op
  }

  val CHSH = A(0)*B(0) + B(1)*A(0) + A(1)*B(0) - A(1)*B(1)

  checkAll("free monoid", RingLaws[FreeBasedMono.Free[FM.type]].multiplicativeMonoid)
  checkAll("free monoid involution", InvolutionLaws[FreeBasedMono.Free[FM.type]].involutionMultiplicativeMonoid)
  checkAll("free binoid", ExtraMultiplicativeMonoidLaws[FreeBasedMono.Free[FM.type]].multiplicativeBinoid)

  val QM = quotient.MonoidDef(FM) {
    case (A(x1), A(x2)) if x1 == x2 => FreeBasedMono.one
    case (B(y1), B(y2)) if y1 == y2 => FreeBasedMono.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  }

  val ambientGroup = QM.groupInQuotient(Grp(swapParties, swapInputA, swapOutputA0))

  val gset: GSet[QM.type] = QM.quotient(GSet.onePlus(A, B))

  val L: Evaluator[QM.type] = QM.evaluator.real
  val gramMatrix = GramMatrix[L.type, QM.type](L, gset)

  checkAll("quotient monoid", RingLaws[FreeBasedMono[QM.type, FM.type]].multiplicativeMonoid)
  checkAll("quotient monoid involution", InvolutionLaws[FreeBasedMono[QM.type, FM.type]].involutionMultiplicativeMonoid)
  checkAll("quotient binoid", ExtraMultiplicativeMonoidLaws[FreeBasedMono[QM.type, FM.type]].multiplicativeBinoid)

  import cyclo.Cyclos.arbCyclo
  implicit val cycloPred: Predicate[Cyclo] = { (c: Cyclo) => !c.isZero}
  checkAll("nc poly ring", spire.laws.RingLaws[Poly[QM.type, FM.type]].ring)
  checkAll("nc ring as vector space", spire.laws.VectorSpaceLaws[Poly[QM.type, FM.type], Cyclo].vectorSpace)
  checkAll("nc ring involution", spire.laws.InvolutionLaws[Poly[QM.type, FM.type]].involutionMultiplicativeMonoid)

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
