package net.alasc.symdpoly

import cyclo.Cyclo

import org.typelevel.discipline.Predicate
import spire.laws.{InvolutionLaws, RingLaws}

import net.alasc.symdpoly.laws.ExtraMultiplicativeMonoidLaws

class CHSHMonoidSuite extends CommonSuite {

  import laws.Monos._
  import laws.Polys._

  object FM extends free.MonoidDef {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianType1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianType1(0 to 1)

    val operators = Seq(A, B)
  }

  import FM.{A, B}

  val swapParties = FM.generator {
    case A(i) => B(i)
    case B(i) => A(i)
  }

  val swapInputA = FM.generator {
    case A(x) => A(1-x)
    case B(y) => B(y)
  }

  val swapOutputA0 = FM.generator {
    case A(0) => A(0)*Phase.minusOne
    case op => op
  }

  val CHSH = A(0)*B(0) + B(1)*A(0) + A(1)*B(0) - A(1)*B(1)

  checkAll("free monoid", RingLaws[Mono.Free[FM.type]].multiplicativeMonoid)
  checkAll("free monoid involution", InvolutionLaws[Mono.Free[FM.type]].involutionMultiplicativeMonoid)
  checkAll("free binoid", ExtraMultiplicativeMonoidLaws[Mono.Free[FM.type]].multiplicativeBinoid)

  val QM = quotient.MonoidDef(FM) {
    case (A(x1), A(x2)) if x1 == x2 => Mono.one
    case (B(y1), B(y2)) if y1 == y2 => Mono.one
    case (B(y), A(x)) => Mono(A(x), B(y))
    case (op1, op2) => Mono(op1, op2)
  }

  val ambientGroup = QM.ambientGroup(swapParties, swapInputA, swapOutputA0)

  val gset = QM.quotient(GSet.onePlus(A, B))

  val gramMatrix = GramMatrix(gset, evaluation.pureStateSelfAdjoint(QM))

  checkAll("quotient monoid", RingLaws[Mono[QM.type, FM.type]].multiplicativeMonoid)
  checkAll("quotient monoid involution", InvolutionLaws[Mono[QM.type, FM.type]].involutionMultiplicativeMonoid)
  checkAll("quotient binoid", ExtraMultiplicativeMonoidLaws[Mono[QM.type, FM.type]].multiplicativeBinoid)

  import cyclo.Cyclos.arbCyclo
  implicit val cycloPred: Predicate[Cyclo] = { (c: Cyclo) => !c.isZero}
  checkAll("nc poly ring", spire.laws.RingLaws[Poly[QM.type, FM.type]].ring)
  checkAll("nc ring as vector space", spire.laws.VectorSpaceLaws[Poly[QM.type, FM.type], Cyclo].vectorSpace)
  checkAll("nc ring involution", spire.laws.InvolutionLaws[Poly[QM.type, FM.type]].involutionMultiplicativeMonoid)

}
