package net.alasc.symdpoly
package examples
package tracial

import spire.syntax.ring._
/*
object BurgdorfExample5_13 {
  /** Free monoid containing the operator variables. */
  object Free extends free.MonoidDef(1) {
    case object X1 extends HermitianSingleOp
    case object X2 extends HermitianSingleOp
    lazy val operators = Seq(X1, X2)
  }

  import Free.{X1, X2}

  val f = (X1.pow(2) + X1.pow(3) * 2 + X1.pow(4) * 2 + X1.pow(6) + 3
    - X1.pow(4) * X2 * 4 + X1.pow(4) * X2.pow(2) + X1.pow(3) * X2 * 4 + X1.pow(3) * X2.pow(2) * 2 - X1.pow(3) * X2.pow(3) * 2
    + X1.pow(2) * X2 * 2 - X1.pow(2) * X2.pow(2) + X1*X2*X1*X2 * 8 + X1.pow(2) * X2.pow(3) * 2 - X1*X2 * 4 + X1*X2.pow(2) * 4 + X1 * X2.pow(4) * 6 - X2 * 2
    + X2.pow(2) - X2.pow(3) * 4 + X2.pow(4) * 2 + X2.pow(6) * 2)

  val L = Free.evaluator(evaluation.cyclicReal(Free))
  val generatingSet = GSet.onePlus(X1, X2).pow(3)
  val problem = L(f).minimize.relaxation(generatingSet)
  val program: sdp.Program = problem.program
}
*/