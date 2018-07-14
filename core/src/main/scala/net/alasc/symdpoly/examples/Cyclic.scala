package net.alasc.symdpoly
package examples

object Cyclic {

  object Free extends free.MonoidDef {

    case class X(i: Int) extends HermitianOp
    object X extends HermitianType1(0 to 3)

    val operators = Seq(X)
  }

  import Free.{X}

  val Quotient = quotient.MonoidDef(Free) {
    case (X(i), X(j)) if i == j => Mono.one
    case (op1, op2) => op1 * op2
  }

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
  val L = evaluation.cyclic(Free)

}
