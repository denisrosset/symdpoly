package net.alasc.symdpoly
package examples

import cyclo.Cyclo

object CHSH {

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

  val bellOperator = Quotient.quotient(A(0)*B(0) + A(0)*B(1) + A(1)*B(0) - A(1)*B(1))

  val generatingSet = Quotient.quotient(GSet.onePlus(A, B))

  val L = evaluation.pureStateSelfAdjoint(Quotient)

  val problem = L(bellOperator).maximize

  val relaxation = problem.symmetricRelaxation(generatingSet, G.grp)

/*relaxation.writeMomentMatrix("chsh_moment_matrix.txt")
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

object CHSHApp extends App {
  import CHSH._

  relaxation.mosekInstance.writeCBF("chsh.cbf")
}

object Distributed {

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
/*    case (A(x1), A(x2)) if x1 > x2 => A(x2) * A(x1)
    case (B(y1), B(y2)) if y1 > y2 => B(y2) * B(y1)*/
    case (op1, op2) => op1 * op2
  }

  def PA(a: Int, x: Int) = a match {
    case 0 => (Poly.one[Free.type, Free.type] + A(x).toMono)/2
    case 1 => (Poly.one[Free.type, Free.type]- A(x).toMono)/2
  }

  def PB(b: Int, y: Int) = b match {
    case 0 => (Poly.one[Free.type, Free.type] + B(y).toMono)/2
    case 1 => (Poly.one[Free.type, Free.type] - B(y).toMono)/2
  }

  def PAB(a: Int, b: Int, x: Int, y: Int) = PA(a,x) * PB(b,y)

  val objective = (for {
    a <- 0 to 1
    b <- 0 to 1 if a == b
    x <- 0 to 1
    y <- 0 to 1
  } yield {
    if (x == y) {
      if (a == x) PAB(a,b,x,y) else Poly.zero[Free.type, Free.type]
    } else PAB(a,b,x,y)
  }).reduce(_ + _)


  val bellOperator = Quotient.quotient(objective)
  println(bellOperator)
  val generatingSet = Quotient.quotient(GSet.onePlus(A, B).pow(4))

  val L = evaluation.pureStateSelfAdjoint(Quotient)

  val problem = L(bellOperator).maximize

  val relaxation = problem.relaxation(generatingSet)

  /*relaxation.writeMomentMatrix("chsh_moment_matrix.txt")
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

object DistributedApp extends App {
  import Distributed._

  relaxation.mosekInstance.writeCBF("distributed.cbf")
}
