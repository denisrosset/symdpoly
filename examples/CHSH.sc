// Computation of the quantum upper bound of the CHSH inequality
//
// We use the correlator notation here

// Let's import the relevant libraries
interp.repositories() :+= coursier.MavenRepository("https://dl.bintray.com/denisrosset/maven")

@

import $ivy.`net.alasc::symdpoly-core:0.5`
import net.alasc.symdpoly._
import defaults._

// Free monoid containing the operator variables
// The cyclotomic order is two so that we can use the roots of unity -1 and +1 when dealing with symmetries
val cyclotomicOrder = 2
object Free extends free.MonoidDef(cyclotomicOrder) {

  case class A(x: Int) extends HermitianOp
  object A extends HermitianOpFamily1(0 to 1)

  case class B(y: Int) extends HermitianOp
  object B extends HermitianOpFamily1(0 to 1)

  val operators = Seq(A, B)
}

import Free.{A, B}

/** Quotient monoid, with the following rules:
  *
  * - A(x)*A(x) = B(y)*B(y) = 1
  * - A(x) and B(y) commute
  */
val Quantum = Free.quotientMonoid(quotient.pairs {
  case (A(x1), A(x2)) if x1 == x2 => Free.one
  case (B(y1), B(y2)) if y1 == y2 => Free.one
  case (B(y), A(x)) => A(x) * B(y)
  case (op1, op2) => op1 * op2
})


// Expression of the CHSH inequality written with correlators
val chsh = Quantum.quotient( A(0)*B(0) + A(0)*B(1) + A(1)*B(0) - A(1)*B(1) )

println(s"We maximize the CHSH expression $chsh")

// Evaluation of monomials: we have trace(f) = trace(f.adjoint) as we can assume all operators/states are real
val L = Quantum.evaluator(evaluation.real)

// Maximization problem
val problem = L(chsh).maximize

val (symmetrizedProblem, group) = problem.symmetrize()

println(s"We discovered a symmetry group of order ${group.order} with generators ${group.generators}")

// Generating set of monomials for the NPA level 1
val level = 1
val generatingSet = Quantum.quotient(GSet.onePlus(A, B)).pow(level)

// Moment-based relaxation
val relaxation = symmetrizedProblem.relaxation(generatingSet)

println(s"Resulting relaxation: $relaxation")

relaxation.program.sdpa.writeFile("chsh.dat-s")
relaxation.program.mosek.writeFile("chsh.cbf")
relaxation.program.sedumi.writeFile("chsh.sedumi.mat")

println("We wrote SDP data description files for SDPA (chsh.dat-s), Mosek (chsh.cbf) and SeDuMi (chsh.sedumi.mat)")
