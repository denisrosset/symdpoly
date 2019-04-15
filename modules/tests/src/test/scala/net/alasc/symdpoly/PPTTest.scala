package net.alasc.symdpoly

import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.symdpoly.examples.quantum.{SliwaData, SliwaInequality}


class PPTTest extends CommonSuite {
  import examples.quantum.Sliwa._

  test("Simple PPT test") {
    import Free.{A, B, C}
    val m1 = LptA(Quotient.quotient(A(0) * A(1) * B(1) * B(0) * C(0) * C(1)))
    val m2 = LptA(Quotient.quotient(A(0) * A(1) * B(0) * B(1) * C(1) * C(0)))
    assert(m1 == m2)
  }


  test("Optimized vs nonoptimized") {
    val generatingSet = localLevel(1)
    def nMonomials(expr: Quotient.PolyType, ev: Evaluator.Aux[Quotient.type]): Int = {
      val (problem: Optimization[ev.type, Quotient.type], _) = ev(expr).maximize.symmetrize()
      problem.relaxation(generatingSet).allMoments.length
    }
    for (i <- 1 to 46; ev <- Seq(L, LptA, LptB, LptC, LptAll)) {
      val expr = SliwaInequality.fromIndex1(i).expression
      val n1 = Settings.withOptimize(false)(nMonomials(expr, ev))
      val n2 = Settings.withOptimize(true)(nMonomials(expr, ev))
      assert(n1 == n2)
    }
  }

}
