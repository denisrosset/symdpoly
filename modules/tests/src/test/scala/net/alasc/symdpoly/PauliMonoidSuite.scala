package net.alasc.symdpoly

import spire.laws.{InvolutionLaws, RingLaws}

import net.alasc.symdpoly
import net.alasc.symdpoly.free.PhasedOpLike
import net.alasc.symdpoly.freebased.Mono
import net.alasc.symdpoly.laws.ExtraMultiplicativeMonoidLaws
import net.alasc.symdpoly.math.Phase

class PauliMonoidSuite extends CommonSuite {

  import laws.Monos._
  import laws.Polys._

  object Free extends free.MonoDef(4) {

    case class σ(i: Int) extends HermitianOp {
      require(i >= 1 && i <= 3)
    }
    object σ extends HermitianOpFamily1(1 to 3)

    lazy val families = Seq(σ)
  }

  import Free.σ

  checkAll("free monoid", RingLaws[Free.MonoType].multiplicativeMonoid)
  checkAll("free monoid involution", InvolutionLaws[Free.MonoType].involutionMultiplicativeMonoid)
  checkAll("free binoid", ExtraMultiplicativeMonoidLaws[Free.MonoType].multiplicativeBinoid)

  def mod1(i: Int, n: Int): Int = ((i - 1) % n) + 1

  // Pauli algebra
  val Quotient = Free.quotientMonoid(quotient.pairs {
    case (σ(i), σ(j)) if i == j => Mono.one
    case (σ(i), σ(j)) if mod1(i + 1, 3) == j => -σ(6 - i - j)*Phase.i
    case (σ(i), σ(j)) if mod1(i + 2, 3) == j => σ(6 - i - j)*Phase.i
  })

  checkAll("quotient monoid", RingLaws[Quotient.MonoType].multiplicativeMonoid)
  checkAll("quotient monoid involution", InvolutionLaws[Quotient.MonoType].involutionMultiplicativeMonoid)
  checkAll("quotient binoid", ExtraMultiplicativeMonoidLaws[Quotient.MonoType].multiplicativeBinoid)

  test("Pauli algebra") {
    Quotient.quotient(σ(1)*σ(2)*σ(3)) shouldBe Quotient.quotient(Free.one*Phase.minusI)
  }

}
