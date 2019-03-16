package net.alasc.symdpoly.examples.quantum

import net.alasc.symdpoly._
import net.alasc.symdpoly.defaults._

/** The I3322 inequality in the scenario with two parties, three measurement settings and binary outcomes. */
object I3322 {

  /** The operator variables are A(x=0,1,2) B(y=0,1,2). */
  object Free extends free.MonoidDef(2) {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianOpFamily1(0 to 2)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianOpFamily1(0 to 2)

    val operators = Seq(A, B)
  }

  import Free.{A, B}

  /** Quotient monoid expressing that A and B commute, and that A(x) and B(y) are projectors
    * with +1/-1 eigenvalues.
    */
  val Quotient = Free.quotientMonoid(quotient.pairs {
    case (A(x1), A(x2)) if x1 == x2 => Free.one
    case (B(y1), B(y2)) if y1 == y2 => Free.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  })

  /** Permutations that preserve the quotient structure. */
  val swapParties = Free.permutation {
    case A(i) => B(i)
    case B(i) => A(i)
  }

  val inputSwapA = Free.permutation {
    case A(0) => A(1)
    case A(1) => A(0)
    case op => op
  }

  val inputCyclicA = Free.permutation {
    case A(0) => A(1)
    case A(1) => A(2)
    case A(2) => A(0)
    case op => op
  }

  val outputA0 = Free.permutation {
    case A(0) => -A(0)
    case op => op
  }

  val feasibilityGroup = Quotient.groupInQuotient(Grp(swapParties, inputSwapA, inputCyclicA, outputA0))

  def generatingSet(npaLevel: Int): GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A, B)).pow(npaLevel)

  val L = Quotient.evaluator(evaluation.real)

  val bellOperator = Quotient.quotient(
    A(2) * B(1) + A(1) * B(2) - A(1) * B(1) - A(0) * B(2) - A(2) * B(0) - A(1) * B(0) - A(0) * B(1) - A(0) * B(0)
     - A(0) - A(1) - B(0) - B(1)
  )/4

  val feasGrp = Quotient.symmetryGroup
  val symGrp = bellOperator.invariantSubgroupOf(feasGrp)
  val Lsym = Quotient.evaluator(evaluation.real, symGrp)
  val problem = Lsym(bellOperator).maximize
}

/** Creates the problem files for I3322 in the Mosek and SDPA formats, relaxation levels 2,3,4,5 */
object I3322App extends App {

  import I3322._
  for (level <- 1 to 5) {
    println(level)
    val relaxation: Relaxation[_, _] = problem.relaxation(generatingSet(level))
    relaxation.program.mosek.writeFile(s"i3322_$level.cbf")
    relaxation.program.sdpa.writeFile(s"i3322_$level.dat-s")
    if (level < 5) {
      // TODO: bring back SCS save when sparse matrix concatenation is faster
      relaxation.program.scs.writeFile(s"i3322_${level}_scs.mat")
      relaxation.program.sedumi.writeFile(s"i3322_${level}_sedumi.mat")
    }
  }
}
