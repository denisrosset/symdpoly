package net.alasc.symdpoly
package examples

import cyclo.Cyclo

import net.alasc.symdpoly.joptimizer._
import net.alasc.symdpoly.matlab._
import defaults._

object CHSH {

  object Free extends free.MonoidDef(2) {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianOpType1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianOpType1(0 to 1)

    val operators = Seq(A, B)
  }

  import Free.{A, B}

  val Quotient = quotient.MonoidDef(Free) {
    case (A(x1), A(x2)) if x1 == x2 => Free.one
    case (B(y1), B(y2)) if y1 == y2 => Free.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  }

  val swapParties = Free.permutation {
    case A(i) => B(i)
    case B(i) => A(i)
  }
  val inputSwapA = Free.permutation {
    case A(0) => A(1)
    case A(1) => A(0)
    case op => op
  }
  val outputSwapA0 = Free.permutation {
    case A(0) => -A(0)
    case op => op
  }

  val ambientGroup = Quotient.groupInQuotient(Grp(swapParties, inputSwapA, outputSwapA0))
  val generators = Seq(swapParties, inputSwapA, outputSwapA0)

  val bellOperator = Quotient.quotient(A(0)*B(0) + A(0)*B(1) + A(1)*B(0) - A(1)*B(1))

  val generatingSet = Quotient.quotient(GSet.onePlus(A, B))

  val L = Quotient.evaluator.real

  val symGroup = ambientGroup.leavesInvariant(L(bellOperator))

  val Lsym = L.symmetric(symGroup)

  val problem = Lsym(bellOperator).maximize

  val relaxation = problem.relaxation(generatingSet)

}

object CHSHApp extends App {
  import CHSH._
  println(relaxation.gramMatrix.momentMatrix)
  println(relaxation.jOptimizerInstance.solve())
  relaxation.sedumiInstance.writeFile("chsh_sedumi.mat")
  relaxation.mosekInstance.writeCBF("chsh.cbf")
}
