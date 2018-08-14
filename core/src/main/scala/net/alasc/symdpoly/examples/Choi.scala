package net.alasc.symdpoly
package examples

import net.alasc.symdpoly.evaluation.Symmetries
import net.alasc.symdpoly.examples.CHSH.Free
import net.alasc.symdpoly.examples.CHSH.Free.{A, B}

object Choi extends App {

  object Free extends free.MonoidDef {

    case class X(i: Int) extends HermitianOp
    object X extends HermitianType1(1 to 3)

    case class Y(i: Int) extends HermitianOp
    object Y extends HermitianType1(1 to 3)
    val operators = Seq(X, Y)

  }

  import Free.{X, Y}
  val Quotient = quotient.MonoidDef(Free) {
    case (X(i), X(j)) if i > j => X(j) * X(i)
    case (Y(i), Y(j)) if i > j => Y(j) * Y(i)
    case (Y(i), X(j)) => X(j) * Y(i)
    case (op1, op2) => op1 * op2
  }

  val B = X(1).pow(2)*Y(1).pow(2) + X(2).pow(2)*Y(2).pow(2) + X(3).pow(2)*Y(3).pow(2) -
    (X(1)*X(2)*Y(1)*Y(2) + X(2)*X(3)*Y(2)*Y(3) + X(3)*X(1)*Y(3)*Y(1)) * 2 +
    (X(1).pow(2)*Y(2).pow(2) + X(2).pow(2)*Y(3).pow(2) + X(3).pow(2)*Y(1).pow(2))

  val S = (X(1).pow(2) + X(2).pow(2) + X(3).pow(2) + Y(1).pow(2) + Y(2).pow(2) + Y(3).pow(2))

  val flipX1 = Free.generator {
    case X(1) => -X(1)
    case op => op
  }

  val cyclic = Free.generator {
    case X(1) => X(2)
    case X(2) => X(3)
    case X(3) => Y(1)
    case Y(1) => Y(2)
    case Y(2) => Y(3)
    case Y(3) => X(1)
  }

  val swapX1X2 = Free.generator {
    case X(1) => X(2)
    case X(2) => X(1)
    case op => op
  }

  val generatingSet = Quotient.quotient(GSet.onePlus(X, Y).pow(3))

  val L = evaluation.pureStateSelfAdjoint(Quotient)

  val ambientGroup = Free.ambientGroup(flipX1, cyclic, swapX1X2)

  val obj = Quotient.quotient(-B*S)
  val problem = L(obj).maximize

  val relaxation = problem.symmetricRelaxation(generatingSet, ambientGroup)
  println(Symmetries.symmetrySubgroup(L(obj), ambientGroup).order)
  relaxation.mosekInstance.writeCBF("choisym.cbf")
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
