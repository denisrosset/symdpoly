package net.alasc.symdpoly.examples

import net.alasc.symdpoly.{AmbientGroup, GSet, Mono, evaluation, free, quotient}

object CHSHScenario extends App {

  object Free extends free.MonoidDef(2) {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianType1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianType1(0 to 1)

    val operators = Seq(A, B)
  }

  import Free.{A, B}

  val Quotient = quotient.MonoidDef(Free) {
    case (A(x1), A(x2)) if x1 == x2 => Mono.one
    case (B(y1), B(y2)) if y1 == y2 => Mono.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  }

  object G extends AmbientGroup[Quotient.type, Free.type] {
    val swapParties = generator {
      case A(i) => B(i)
      case B(i) => A(i)
    }
    val inputSwapA = generator {
      case A(0) => A(1)
      case A(1) => A(0)
      case op => op
    }
    val outputSwapA0 = generator {
      case A(0) => -A(0)
      case op => op
    }
    val generators = Seq(swapParties, inputSwapA, outputSwapA0)
  }

  val bellOperator = Quotient.quotient(A(0) + A(0)*B(0) + A(0)*B(1) + A(1)*B(0) - A(1)*B(1))

  val generatingSet = Quotient.quotient(GSet.onePlus(A) * GSet.onePlus(B))

  val L = evaluation.pureStateSelfAdjoint(Quotient)

  val problem = L(bellOperator).maximize

  val relaxation = problem.symmetricRelaxation(generatingSet, G.grp)

  println(relaxation.momentIndexMatrixDescription)
  println(relaxation.phaseMatrixDescription)
  println(relaxation.momentMatrixDescription)
  println(relaxation.canonicalMonomialsDescription)
}
