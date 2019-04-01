package net.alasc.symdpoly.examples.quantum

import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.symdpoly.{GSet, Optimization, Relaxation, evaluation, generic, quotient}

object CHSHApp extends App {
  import CHSH._
  import Free.{A, B}

  type RelaxationM[M <: generic.MonoidDef with Singleton] = Relaxation[_ <: Evaluator.Aux[M] with Singleton, M]
  type ProblemM[M <: generic.MonoidDef with Singleton] = Optimization[_ <: Evaluator.Aux[M] with Singleton, M]

  def bellOperator(M: quotient.MonoidDef.Aux[Free.type]): M.PolyType =
    M.quotient(chsh)

  def relaxation(M: quotient.MonoidDef.Aux[Free.type])(generatingSet: GSet[M.type]): (RelaxationM[M.type], RelaxationM[M.type]) = {
    /** Default evaluator. */
    val L = M.evaluator(evaluation.real)

    /** Relaxation with all monomials of given local Level. */
    //    val generatingSet = M.quotient(freeGeneratingSet)

    /** Maximization problem. */
    val problem = L(bellOperator(M)).maximize

    /** Relaxation. */
    val relaxation: RelaxationM[M.type] = problem.relaxation(generatingSet)

    /** Automatic symmetrization. */
    val (problemAuto: ProblemM[M.type], _) = problem.symmetrize()

    val relaxationAuto = problemAuto.relaxation(generatingSet)

    (relaxation, relaxationAuto)
  }

  val (classical, classicalAuto) = relaxation(Classical)(Classical.quotient(GSet.onePlus(A) * GSet.onePlus(B)))
  println("Classical case")
  println("==============")
  // Optimization disabled as KKT conditions fail in JOptimizer
  //  println("The non symmetrized problem gives (classical)")
  //  println(classical)
  //  println(classical.program.jOptimizer.solve())
  println("The automatic symmetrization gives (classical)")
  println(classicalAuto)
  println(classicalAuto.program.jOptimizer.solve())

  val (quantum, quantumAuto) = relaxation(Quantum)(Quantum.quotient(GSet.onePlus(A, B)))
  println("Quantum case")
  println("============")
  println("The non symmetrized problem gives (quantum)")
  println(quantum)
  println(quantum.program.jOptimizer.solve())
  println("while the automatic symmetrization gives (quantum)")
  println(quantumAuto)
  println(quantumAuto.program.jOptimizer.solve())

}
