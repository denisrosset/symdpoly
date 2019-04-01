package net.alasc.symdpoly
package examples.quantum

import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.Evaluator
import shapeless.Witness
import spire.syntax.cfor._
import net.alasc.perms.default._


class SliwaBounds extends CommonSuite {

  test("Almost quantum bounds, including PPT, are reproduced") {
    import Sliwa._

    val generatingSet = localLevel(1)
    //  println(localLevel(6).toSortedSet.size)
    val tol = 1e-3

    cforRange(0 until 46) { index0 =>
      val index1 = index0 + 1
      val sliwa = SliwaInequality.fromIndex0(index0)
      println(s"Working on inequality #$index1")
      val expression = sliwa.expression
      println(s"Expression: $expression")
      val obj = -expression
      val symmetryGroup = obj.invariantSubgroupOf(feasibilityGroup)
      println(s"Symmetry group order: ${symmetryGroup.order}")

      def solve(E: Evaluator.Aux[Quotient.type], suffix: String, optPaper: Double): Unit = {
        implicit def witnessE: Witness.Aux[E.type] = E.witness

        val problem = E(obj).maximize
        val relaxation = problem.relaxation(generatingSet)
        val relaxationSym = problem.symmetrize().relaxation(generatingSet)
        import net.alasc.symdpoly.mosek._
        val OptimumFound(_, opt) = relaxation.program.nativeMosek.solve()
        val OptimumFound(_, optSym) = relaxationSym.program.nativeMosek.solve()
        println(s"Inequality: $index1$suffix Bound: $opt Symmetrized bound: $optSym Paper bound: $optPaper")
        println(s"Number of unique monomials: ${relaxation.allMoments.length}")
        assert(spire.math.abs(opt - optPaper) < tol)
        assert(spire.math.abs(opt - optSym) < tol)
        println("")
      }

      solve(L, "", sliwa.almostQuantum)
      solve(LptA, "_ptA", sliwa.almostQuantumTA)
      solve(LptB, "_ptB", sliwa.almostQuantumTB)
      solve(LptC, "_ptC", sliwa.almostQuantumTC)
      solve(LptAll, "_ptABC", sliwa.almostQuantumTall)
    }
  }

}
