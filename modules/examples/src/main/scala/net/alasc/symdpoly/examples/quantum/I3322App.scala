package net.alasc.symdpoly.examples.quantum

import net.alasc.symdpoly.examples.quantum.I3322.{Quotient, bellOperator}
import net.alasc.symdpoly.{Relaxation, evaluation}

/** Creates the problem files for I3322 in the Mosek and SDPA formats, relaxation levels 2,3,4,5 */
object I3322App extends App {

  val feasGrp = Quotient.symmetryGroup
  val symGrp = bellOperator.invariantSubgroupOf(feasGrp)
  val Lsym = Quotient.symmetricEvaluator(symGrp, evaluation.real)
  val problem = Lsym(bellOperator).maximize

  import I3322._
  for (level <- 1 to 5) {
    println(level)
    val relaxation: Relaxation[_, _] = problem.relaxation(generatingSet(level))
    relaxation.program.mosek.writeFile(s"i3322_$level.cbf")
    relaxation.program.sdpa.writeFile(s"i3322_$level.dat-s")
    relaxation.program.scs.writeFile(s"i3322_${level}_scs.mat")
    relaxation.program.sedumi.writeFile(s"i3322_${level}_sedumi.mat")
    relaxation.program.sdpt3.writeFile(s"i3322_${level}_sdpt3.mat")
  }
}
