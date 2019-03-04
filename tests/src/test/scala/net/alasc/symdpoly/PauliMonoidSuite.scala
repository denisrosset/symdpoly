package net.alasc.symdpoly

import spire.laws.{InvolutionLaws, RingLaws}

import net.alasc.symdpoly
import net.alasc.symdpoly.generic.FreeBasedMono
import net.alasc.symdpoly.laws.ExtraMultiplicativeMonoidLaws

class PauliMonoidSuite extends CommonSuite {

  import laws.Monos._
  import laws.Polys._

  object FM extends free.MonoidDef(4) {

    case class σ(i: Int) extends HermitianOp {
      require(i >= 1 && i <= 3)
    }
    object σ extends HermitianOpType1(1 to 3)

    val operators = Seq(σ)
  }

  import FM.σ

  checkAll("free monoid", RingLaws[FreeBasedMono.Free[FM.type]].multiplicativeMonoid)
  checkAll("free monoid involution", InvolutionLaws[FreeBasedMono.Free[FM.type]].involutionMultiplicativeMonoid)
  checkAll("free binoid", ExtraMultiplicativeMonoidLaws[FreeBasedMono.Free[FM.type]].multiplicativeBinoid)

  def mod1(i: Int, n: Int): Int = ((i - 1) % n) + 1

  // Pauli algebra
  val QM = quotient.MonoidDef(FM)(pairSubstitutions = {
    case (σ(i), σ(j)) if i == j => FreeBasedMono.one
    case (σ(i), σ(j)) if mod1(i + 1, 3) == j => -σ(6 - i - j)*Phase.i
    case (σ(i), σ(j)) if mod1(i + 2, 3) == j => σ(6 - i - j)*Phase.i
  })

  checkAll("quotient monoid", RingLaws[FreeBasedMono[QM.type, FM.type]].multiplicativeMonoid)
  checkAll("quotient monoid involution", InvolutionLaws[FreeBasedMono[QM.type, FM.type]].involutionMultiplicativeMonoid)
  checkAll("quotient binoid", ExtraMultiplicativeMonoidLaws[FreeBasedMono[QM.type, FM.type]].multiplicativeBinoid)

  test("Pauli algebra") {
    QM.quotient(σ(1)*σ(2)*σ(3)) shouldBe QM.quotient(FM.one*Phase.minusI)
  }

}
