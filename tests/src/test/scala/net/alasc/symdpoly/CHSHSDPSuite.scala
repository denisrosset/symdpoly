package net.alasc.symdpoly

import joptimizer._
import defaults._

class CHSHSDPSuite extends CommonSuite {

  test("Test CHSH Tsirelson bound") {
    object Free extends free.MonoidDef(2) {

      case class A(x: Int) extends HermitianOp
      object A extends HermitianOpFamily1(0 to 1)

      case class B(y: Int) extends HermitianOp
      object B extends HermitianOpFamily1(0 to 1)

      val operators = Seq(A, B)
    }

    import Free.{A, B}

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

    val Quotient = Free.quotientMonoid(quotient.pairs {
      case (A(x1), A(x2)) if x1 == x2 => Free.one
      case (B(y1), B(y2)) if y1 == y2 => Free.one
      case (B(y), A(x)) => A(x) * B(y)
      case (op1, op2) => op1 * op2
    })

    val bellOperator = Quotient.quotient(A(0)*B(0) + A(0)*B(1) + A(1)*B(0) - A(1)*B(1))

    val freeGroup = Free.symmetryGroup
    val feasibilityGroup = Quotient.groupInQuotientNC(Grp(swapParties, inputSwapA, outputSwapA0))
    val quotientGroup = Quotient.groupInQuotient(freeGroup)

    assert(feasibilityGroup === quotientGroup)

    val generatingSet = Quotient.quotient(GSet.onePlus(A, B))

    val L = Quotient.evaluator(evaluation.real)

    val symmetryGroup = feasibilityGroup.leavesInvariant(L(bellOperator))

    val Lsym = Quotient.evaluator(evaluation.real, evaluation.symmetric(symmetryGroup))

    val problem = Lsym(bellOperator).maximize

    val relaxation = problem.relaxation(generatingSet)
    import net.alasc.symdpoly.matlab._

    val OptimumFound(_, ub, _, _) = relaxation.jOptimizerInstance.solve()

    import spire.math.{abs, sqrt}
    val tol = 1e-9
    assert(abs(sqrt(8.0) - ub) < sqrt(tol))

  }

}
