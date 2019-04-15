package net.alasc.symdpoly.examples.quantum

import net.alasc.symdpoly.examples.quantum.I3322.{Quotient, bellOperator}
import net.alasc.symdpoly.{Relaxation, Settings, evaluation}
import scalin.immutable.dense._
/** Creates the problem files for I3322 in the Mosek and SDPA formats, relaxation levels 2,3,4,5 */
object I3322App {


  def main(args: Array[String]): Unit = {

    val feasGrp = Quotient.symmetryGroup
    val symGrp = bellOperator.invariantSubgroupOf(feasGrp)
    val Lsym = Quotient.eigenvalueEvaluator(true).symmetrize(symGrp)
    val problem = Lsym(bellOperator).maximize

    import I3322._
    for (level <- 6 to 6) {
      val relaxation: Relaxation[_, _] = problem.relaxation(generatingSet(level))
      relaxation.program.sedumi.writeFile(s"i3322_${level}_sedumi.mat")
      relaxation.program.mosek.writeFile(s"i3322_$level.cbf")
      relaxation.program.sdpa.writeFile(s"i3322_$level.dat-s")
      relaxation.program.scs.writeFile(s"i3322_${level}_scs.mat")
      relaxation.program.sdpt3.writeFile(s"i3322_${level}_sdpt3.mat")
    }
  }
}
