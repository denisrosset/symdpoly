package net.alasc.symdpoly
package examples

import net.alasc.symdpoly.math.{GenPerm, PhasedInt}

object I3322 {

  object FM extends free.MonoidDef(2) {

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

  val generatingSet = QM.quotient(GSet.onePlus(A, B)).pow(2)

  val L = evaluation.pureStateSelfAdjoint(QM)
  val L1 = QM.evaluator.adjoint

  val bellOperator = QM.quotient(
    A(2) * B(1) + A(1) * B(2) - A(1) * B(1) - A(0) * B(2) - A(2) * B(0) - A(1) * B(0) - A(0) * B(1) - A(0) * B(0)
     - A(0) - A(1) - B(0) - B(1)
  )/4

  val feasGrp = QM.restrictedGroup(FM.symmetryGroup2)
  val symGrp = feasGrp.leavesInvariant(L1(bellOperator))

  val problem = L(bellOperator).maximize
  val relaxation = problem.symmetricRelaxation(generatingSet, ambientGroup)

  val L1sym = L1.symmetric(symGrp)
  val problem1 = L1sym(bellOperator).maximize
  val relaxation1 = problem1.relaxation(generatingSet)

/*
  relaxation.writeMomentMatrix("i3322_moment_matrix.txt")
  relaxation.writePhaseMatrix("i3322_phase_matrix.txt")
  relaxation.writeMomentIndexMatrix("i3322_moment_index_matrix.txt")
  relaxation.writeCanonicalMonomials("i3322_canonical_monomials.txt")
  relaxation.mosekInstance.writeCBF("i3322.cbf")
  relaxation.writeSymmetryGroupDescription("i3322_symmetry_group.mat")
  relaxation.mosekInstance.writeFile("i3322.task")
  relaxation.mosekInstance.writeFile("i3322.jtask")
  relaxation.sdpaInstance.writeFile("i3322.dat-s")
  relaxation.sedumiInstance.writeFile("i3322_sedumi.mat")
  relaxation.scsInstance.writeFile("i3322_scs.mat")
  relaxation.sdpt3Instance.writeFile("i3322_sdpt3.mat")
  */

}

object I3322A extends App {

  object FM extends free.MonoidDef(2) {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianType1(0 to 2)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianType1(0 to 2)

    val operators = Seq(A, B)
  }

  import FM.{A, B}

  val QM = quotient.MonoidDef(FM) {
    case (A(x1), A(x2)) if x1 == x2 => Mono.one
    case (B(y1), B(y2)) if y1 == y2 => Mono.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  }

  val generatingSet = QM.quotient(GSet.onePlus(A, B)).pow(4)

  val L = evaluation.pureStateSelfAdjoint(QM)

  val bellOperator = QM.quotient(
    A(2) * B(1) + A(1) * B(2) - A(1) * B(1) - A(0) * B(2) - A(2) * B(0) - A(1) * B(0) - A(0) * B(1) - A(0) * B(0)
      - A(0) - A(1) - B(0) - B(1)
  )/4

  val problem = L(bellOperator).maximize
  val relaxation: Relaxation[L.type, QM.type, FM.type] = problem.symmetricRelaxation(generatingSet)
  def matlabImage(g: GenPerm): Array[Int] = {
    Array.tabulate[Int](relaxation.gramMatrix.matrixSize) { pIndex =>
      g.image(PhasedInt(Phase.one, pIndex)) match {
        case PhasedInt(Phase.one, iIndex) => iIndex + 1
        case PhasedInt(Phase.minusOne, iIndex) => -(iIndex + 1)
      }
    }
  }
  println(relaxation.objectiveVector)
  println(relaxation.phaseMatrixDescription)
  println(relaxation.momentIndexMatrixDescription)
  println(relaxation.gramMatrix.matrixSymmetries.generators.map(g => matlabImage(g).mkString("["," ","]")))
  println(relaxation.gramMatrix.nUniqueMonomials)
  relaxation.mosekInstance.writeCBF("level1.cbf")
  println(relaxation.canonicalMonomialsDescription)
  /*
    relaxation.writeMomentMatrix("i3322_moment_matrix.txt")
    relaxation.writePhaseMatrix("i3322_phase_matrix.txt")
    relaxation.writeMomentIndexMatrix("i3322_moment_index_matrix.txt")
    relaxation.writeCanonicalMonomials("i3322_canonical_monomials.txt")
    relaxation.mosekInstance.writeCBF("i3322.cbf")
    relaxation.writeSymmetryGroupDescription("i3322_symmetry_group.mat")
    relaxation.mosekInstance.writeFile("i3322.task")
    relaxation.mosekInstance.writeFile("i3322.jtask")
    relaxation.sdpaInstance.writeFile("i3322.dat-s")
    relaxation.sedumiInstance.writeFile("i3322_sedumi.mat")
    relaxation.scsInstance.writeFile("i3322_scs.mat")
    relaxation.sdpt3Instance.writeFile("i3322_sdpt3.mat")
    */

}
