package net.alasc.symdpoly
package matlab

import net.alasc.symdpoly.math.{GenPerm, PhasedInt}

object I3322Matlab extends App {

  object FM extends free.MonoidDef {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianType1(0 to 2)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianType1(0 to 2)

    val operators = Seq(A, B)
  }

  import FM.{A, B}

  val swapParties = FM.generator {
    case A(i) => B(i)
    case B(i) => A(i)
  }

  val inputSwapA = FM.generator {
    case A(0) => A(1)
    case A(1) => A(0)
    case op => op
  }

  val inputCyclicA = FM.generator {
    case A(0) => A(1)
    case A(1) => A(2)
    case A(2) => A(0)
    case op => op
  }

  val outputA0 = FM.generator {
    case A(0) => -A(0)
    case op => op
  }

  val QM = quotient.MonoidDef(FM) {
    case (A(x1), A(x2)) if x1 == x2 => Mono.one
    case (B(y1), B(y2)) if y1 == y2 => Mono.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  }

  val ambientGroup = QM.ambientGroup(swapParties, inputSwapA, inputCyclicA, outputA0)
  val L = evaluation.pureStateSelfAdjoint(QM)

  val bellOperator = QM.quotient(
    A(2) * B(1) + A(1) * B(2) - A(1) * B(1) - A(0) * B(2) - A(2) * B(0) - A(1) * B(0) - A(0) * B(1) - A(0) * B(0)
      - A(0) - A(1) - B(0) - B(1)
  ) / 4

  val problem = L(bellOperator).maximize()

  (1 to 5).foreach { localLevel =>
    val generatingSet = QM.quotient(GSet.onePlus(A, B)).pow(localLevel)

//    val relaxation = problem.symmetricRelaxation(generatingSet, ambientGroup)
//    relaxation.sedumiInstance.writeFile(s"i3322_sym_$localLevel.mat")

    val relaxation1 = problem.relaxation(generatingSet)
    relaxation1.sedumiInstance.writeFile(s"i3322_nonsym_$localLevel.mat")
    relaxation1.sdpaInstance.writeFile(s"i3322_nonsym_$localLevel.dat-s")
  }

}
