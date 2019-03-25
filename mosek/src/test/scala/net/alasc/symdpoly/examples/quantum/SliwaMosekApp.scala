package net.alasc.symdpoly
package examples.quantum

import net.alasc.symdpoly.evaluation.Evaluator
import shapeless.Witness
import spire.syntax.cfor._

object SliwaMosekApp extends App {

  import Sliwa._

  val generatingSet = localLevel(1)
  //  println(localLevel(6).toSortedSet.size)
  val tol = 1e-3

  cforRange(0 until 46) { index0 =>
    val sliwa = SliwaInequality.fromIndex0(index0)
    println(s"Working on inequality #${index0+1}")
    val expression = sliwa.expression
    println(s"Expression: $expression")
    val obj = -expression
    val symmetryGroup = obj.invariantSubgroupOf(feasibilityGroup)
    println(s"Symmetry group order: ${symmetryGroup.order}")

    def solve(E: Evaluator.Aux[Quotient.type], suffix: String, optPaper: Double): Unit = {
      implicit def witnessE: Witness.Aux[E.type] = E.witness
      val problem = E(obj).maximize
      val relaxation = problem.relaxation(generatingSet)
      val relaxationSym = problem.symmetrize(quotientFeasibilityGroup = Some(feasibilityGroup)).relaxation(generatingSet)
      import net.alasc.symdpoly.mosek._
      val OptimumFound(_, opt) = relaxation.program.nativeMosek.solve()
      val OptimumFound(_, optSym) = relaxationSym.program.nativeMosek.solve()
      //relaxation.program.mosek.writeFile(s"sliwa_$index1$suffix.cbf")
      println(s"Number of unique monomials: ${relaxation.allMoments.length}")
      //      val OptimumFound(_, opt) = relaxation.program.jOptimizer.solve(1e-6)
      println(s"$index0$suffix $opt $optSym $optPaper")
      //assert(spire.math.abs(opt - optPaper) < tol)
      println("")
    }
    solve(L, "", sliwa.quantumBound)
    solve(LptA, "_ptA", sliwa.almostQuantumTA)
    solve(LptB, "_ptB", sliwa.almostQuantumTB)
    solve(LptC, "_ptC", sliwa.almostQuantumTC)
    solve(LptAll, "_ptABC", sliwa.almostQuantumTall)
  }

}
