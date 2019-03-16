package net.alasc.symdpoly
package examples.quantum

import net.alasc.symdpoly.defaults._
import net.alasc.symdpoly.joptimizer._
import net.alasc.symdpoly.matlab._

/** Computes the Tsirelson bound on the CHSH inequality, written using
  * correlators A(x) and B(y).
  */
object CHSH {

  /** Free monoid containing the operator variables. */
  object Free extends free.MonoidDef(2) {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianOpFamily1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianOpFamily1(0 to 1)

    val operators = Seq(A, B)
  }

  import Free.{A, B}

  /** Quotient monoid, with the following rules:
    *
    * - A(x) and B(y) commute
    * - A(x)*A(x) = B(y)*B(y) = 1
    */
  val Quotient = Free.quotientMonoid(quotient.pairs {
    case (A(x1), A(x2)) if x1 == x2 => Free.one
    case (B(y1), B(y2)) if y1 == y2 => Free.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  })

  /** Symmetry group generator: permutation of parties. */
  val swapParties = Free.permutation {
    case A(i) => B(i)
    case B(i) => A(i)
  }

  /** Symmetry group generator: permutation of Alice's inputs. */
  val inputSwapA = Free.permutation {
    case A(0) => A(1)
    case A(1) => A(0)
    case op => op
  }

  /** Symmetry group generator: permutation of Alice's outputs for x = 0. */
  val outputSwapA0 = Free.permutation {
    case A(0) => -A(0)
    case op => op
  }

  /** Group that preserves the quotient structure. */
  val feasibilityGroup = Quotient.groupInQuotient(Grp(swapParties, inputSwapA, outputSwapA0))

  /** CHSH expression. */
  val bellOperator = Quotient.quotient(A(0)*B(0) + A(0)*B(1) + A(1)*B(0) - A(1)*B(1))

  /** Default evaluator. */
  val L = Quotient.evaluator(evaluation.real)

  /** Problem symmetry group. */
  val symmetryGroup = bellOperator.invariantSubgroupOf(feasibilityGroup)

  /** Monomial evaluator invariant under the problem symmetry group. */
  val Lsym = Quotient.evaluator(evaluation.real, symmetryGroup)

  /** Relaxation with all monomials of maximal degree 1. */
  val generatingSet = Quotient.quotient(GSet.onePlus(A, B))

  /** Maximization problem. */
  val problem = Lsym(bellOperator).maximize

  /** Relaxation. */
  val relaxation = problem.relaxation(generatingSet)

  /** Automatic symmetrization. */
  val relaxationAuto = L(bellOperator).maximize.symmetrize.relaxation(generatingSet)

}

object CHSHApp extends App {
  import CHSH._
  println("The manual symmetrization gives")
  println(relaxation)
  println(relaxation.jOptimizerInstance.solve())
  println("while the automatic symmetrization gives")
  println(relaxationAuto)
  println(relaxationAuto.jOptimizerInstance.solve())
  relaxation.program.sdpa.writeFile("chsh.dat-s")
  relaxation.program.mosek.writeFile("chsh.cbf")
  relaxation.program.scs.writeFile("chsh_scs.mat")
  relaxation.program.sedumi.writeFile("chsh_sedumi.mat")
  relaxation.program.sdpt3.writeFile("chsh_sdpt3.mat")
}
