package net.alasc.symdpoly

import spire.laws.{InvolutionLaws, RingLaws}

import net.alasc.symdpoly
import net.alasc.symdpoly.freebased.Mono
import net.alasc.symdpoly.laws.ExtraMultiplicativeMonoidLaws
import net.alasc.symdpoly.math.Phase

class PauliMonoidSuite extends CommonSuite {

  import laws.Monos._
  import laws.Polys._

  object Free extends free.MonoidDef(4) {

    case class σ(i: Int) extends HermitianOp {
      require(i >= 1 && i <= 3)
    }
    object σ extends HermitianOpFamily1(1 to 3)

    val operators = Seq(σ)
  }

  import Free.σ

  checkAll("free monoid", RingLaws[Mono.Free[Free.type]].multiplicativeMonoid)
  checkAll("free monoid involution", InvolutionLaws[Mono.Free[Free.type]].involutionMultiplicativeMonoid)
  checkAll("free binoid", ExtraMultiplicativeMonoidLaws[Mono.Free[Free.type]].multiplicativeBinoid)

  def mod1(i: Int, n: Int): Int = ((i - 1) % n) + 1

  // Pauli algebra
  val Quotient = Free.quotientMonoid(quotient.pairs {
    case (σ(i), σ(j)) if i == j => Mono.one
    case (σ(i), σ(j)) if mod1(i + 1, 3) == j => -σ(6 - i - j)*Phase.i
    case (σ(i), σ(j)) if mod1(i + 2, 3) == j => σ(6 - i - j)*Phase.i
  })

  checkAll("quotient monoid", RingLaws[Mono[Quotient.type, Free.type]].multiplicativeMonoid)
  checkAll("quotient monoid involution", InvolutionLaws[Mono[Quotient.type, Free.type]].involutionMultiplicativeMonoid)
  checkAll("quotient binoid", ExtraMultiplicativeMonoidLaws[Mono[Quotient.type, Free.type]].multiplicativeBinoid)

  test("Pauli algebra") {
    Quotient.quotient(σ(1)*σ(2)*σ(3)) shouldBe Quotient.quotient(Free.one*Phase.minusI)
  }

}
