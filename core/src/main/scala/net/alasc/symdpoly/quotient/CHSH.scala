package net.alasc.symdpoly
package quotient

import net.alasc.symdpoly
import net.alasc.symdpoly.Mono

object CHSH {

  object FM extends free.MonoidDef {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianType1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianType1(0 to 1)

    val operators = Seq(A, B)
  }

  import FM.{A, B}

  val CHSH = A(0)*B(0) + B(0)*A(1) + A(1)*B(0) - A(1)*B(1)

  println(CHSH)

  val QM = quotient.MonoidDef(FM) {
    case (A(x1), A(x2)) if x1 == x2 => Mono.one
    case (B(y1), B(y2)) if y1 == y2 => symdpoly.Mono.one
    case (B(y), A(x)) => Mono(A(x), B(y))
    case (op1, op2) => Mono(op1, op2)
  }

  println(QM.quotient(CHSH))

}
