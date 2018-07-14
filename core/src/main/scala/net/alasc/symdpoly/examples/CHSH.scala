package net.alasc.symdpoly
package examples

object CHSH {

  object FM extends free.MonoidDef {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianType1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianType1(0 to 1)

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

  val outputSwapA0 = FM.generator {
    case A(0) => -A(0)
    case op => op
  }

  val QM = quotient.MonoidDef(FM) {
    case (A(x1), A(x2)) if x1 == x2 => Mono.one
    case (B(y1), B(y2)) if y1 == y2 => Mono.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  }

  val bellOperator = QM.quotient(A(0)*B(0) + A(0)*B(1) + A(1)*B(0) - A(1)*B(1))

  val ambientGroup = FM.ambientGroup(swapParties, inputSwapA, outputSwapA0)

  val generatingSet = QM.quotient(GSet.onePlus(A, B))

  val L = evaluation.pureStateSelfAdjoint(QM)

  val problem = L(bellOperator).maximize

  val relaxation = problem.symmetricRelaxation(generatingSet, ambientGroup)

/*  relaxation.writeMomentMatrix("chsh_moment_matrix.txt")
  relaxation.writePhaseMatrix("chsh_phase_matrix.txt")
  relaxation.writeMomentIndexMatrix("chsh_moment_index_matrix.txt")
  relaxation.writeCanonicalMonomials("chsh_canonical_monomials.txt")
  relaxation.writeSymmetryGroupDescription("chsh_symmetry_group.txt")
  relaxation.mosekInstance.writeCBF("chsh.cbf")*/
  /*
  relaxation.mosekInstance.writeFile("chsh.task")
  relaxation.mosekInstance.writeFile("chsh.jtask")
  relaxation.sdpaInstance.writeFile("chsh.dat-s")
  relaxation.sedumiInstance.writeFile("chsh_sedumi.mat")
  relaxation.scsInstance.writeFile("chsh_scs.mat")
  relaxation.sdpt3Instance.writeFile("chsh_sdpt3.mat")
   */

}
