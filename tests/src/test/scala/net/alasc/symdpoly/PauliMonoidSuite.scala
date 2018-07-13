package net.alasc.symdpoly

import spire.laws.{InvolutionLaws, RingLaws}

import net.alasc.symdpoly
import net.alasc.symdpoly.laws.ExtraMultiplicativeMonoidLaws

class PauliMonoidSuite extends CommonSuite {

  import laws.Monos._
  import laws.Polys._

  object FM extends free.MonoidDef {

    case class σ(i: Int) extends HermitianOp {
      require(i >= 1 && i <= 3)
    }
    object σ extends HermitianType1(1 to 3)

    val operators = Seq(σ)
  }

  import FM.σ

  checkAll("free monoid", RingLaws[Mono.Free[FM.type]].multiplicativeMonoid)
  checkAll("free monoid involution", InvolutionLaws[Mono.Free[FM.type]].involutionMultiplicativeMonoid)
  checkAll("free binoid", ExtraMultiplicativeMonoidLaws[Mono.Free[FM.type]].multiplicativeBinoid)

  def mod1(i: Int, n: Int): Int = ((i - 1) % n) + 1

  // Pauli algebra
  val QM = quotient.MonoidDef(FM)(pairSubstitutions = {
    case (σ(i), σ(j)) if i == j => symdpoly.Mono.one
    case (σ(i), σ(j)) if mod1(i + 1, 3) == j => -Mono(σ(6 - i - j))*Phase.i
    case (σ(i), σ(j)) if mod1(i + 2, 3) == j => Mono(σ(6 - i - j))*Phase.i
  })

  checkAll("quotient monoid", RingLaws[Mono[QM.type, FM.type]].multiplicativeMonoid)
  checkAll("quotient monoid involution", InvolutionLaws[Mono[QM.type, FM.type]].involutionMultiplicativeMonoid)
  checkAll("quotient binoid", ExtraMultiplicativeMonoidLaws[Mono[QM.type, FM.type]].multiplicativeBinoid)

  test("Pauli algebra") {
    QM.quotient(σ(1)*σ(2)*σ(3)) shouldBe QM.quotient(symdpoly.Mono.one[FM.type, FM.type]*Phase.minusI)
  }

}
