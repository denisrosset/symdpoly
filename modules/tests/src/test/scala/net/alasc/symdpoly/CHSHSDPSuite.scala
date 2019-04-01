package net.alasc.symdpoly


import defaults._
import net.alasc.symdpoly.util.OrderedSet

class CHSHSDPSuite extends CommonSuite {

  test("Test cardinality of generating set of monomials") {
    import examples.quantum.CHSH._
    // for A(0), A(1) with the relation A(0)^2 = A(1)^2 = 1
    // we only have strings of the form
    // A0 A1 A0 A1 ...
    // A1 A0 A1 A0 ...
    // so the cardinality is

    // for level 0: 1
    // for level 1: 3
    // for level 2: 5
    // for level l: 1 + 2l
    cforRange(0 until 7) { l =>
      val g = Quantum.quotient(GSet.onePlus(Free.A).pow(l))
      val m = g.toOrderedSet
      assert(m.length == 2*l + 1)
    }
  }

  test("Test CHSH scenario feasibility group") {

    import examples.quantum.CHSH._

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

    val feasibilityGroup = Quantum.groupInQuotientNC(Grp(swapParties, inputSwapA, outputSwapA0))
    val feasibilityGroup1 = Quantum.symmetryGroup

    assert(feasibilityGroup === feasibilityGroup1)
  }

  test("CHSH optimal value, non symmetric version") {
    import examples.quantum.CHSH._
    import Free.{A, B}
    val bellOperator = Quantum.quotient(chsh)
    val generatingSet = Quantum.quotient(GSet.onePlus(A, B))
    val L = Quantum.evaluator(evaluation.real)
    val relaxation: Relaxation[L.type, Quantum.type] = L(bellOperator).maximize.relaxation(generatingSet)
    // 1, A0, A1, B0, B1, A0B0, A0B1, A1B0, A1B1, A0A1, B0B1
    assert(relaxation.allMoments.length == 11)
    val OptimumFound(_, ub) = relaxation.program.jOptimizer.solve()
    import spire.math.{abs, sqrt}
    val tol = 1e-9
    assert(abs(sqrt(8.0) - ub) < sqrt(tol))
  }

  test("CHSH optimal value, symmetric version") {
    import examples.quantum.CHSH._
    import Free.{A, B}
    val bellOperator = Quantum.quotient(chsh)
    val generatingSet = Quantum.quotient(GSet.onePlus(A, B))
    val L = Quantum.evaluator(evaluation.real)
    val (problemSym, _) = L(bellOperator).maximize.symmetrize()
    val relaxation: Relaxation[_, Quantum.type] = problemSym.relaxation(generatingSet)
    val OptimumFound(_, ub) = relaxation.program.jOptimizer.solve()
    import spire.math.{abs, sqrt}
    val tol = 1e-9
    assert(abs(sqrt(8.0) - ub) < sqrt(tol))
  }

}
