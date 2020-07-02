// Computation of the quantum upper bound of the CHSH inequality
//
// We use the correlator notation here
//
// We only exploit the symmetry group S2 that permutes Alice and Bob

// Let's import the relevant libraries
interp.repositories() :+= coursierapi.MavenRepository.of("https://dl.bintray.com/denisrosset/maven")

@

import $ivy.`net.alasc::symdpoly-core:0.7.6`
import net.alasc.symdpoly._
import defaults._

// Free monoid containing the operator variables
// The cyclotomic order is two so that we can use the roots of unity -1 and +1 when dealing with symmetries
val cyclotomicOrder = 2
object Free extends free.MonoDef(cyclotomicOrder) {

  case class A(x: Int) extends HermitianOp
  object A extends HermitianOpFamily1(0 to 1)

  case class B(y: Int) extends HermitianOp
  object B extends HermitianOpFamily1(0 to 1)

  lazy val families = Seq(A, B)
}

import Free.{A, B}

/** Quotient monoid, with the following rules:
  *
  * - A(x)*A(x) = B(y)*B(y) = 1
  * - A(x) and B(y) commute
  */
val Quotient = Free.quotientMonoid(quotient.pairs {
  case (A(x1), A(x2)) if x1 == x2 => Free.one
  case (B(y1), B(y2)) if y1 == y2 => Free.one
  case (B(y), A(x)) => A(x) * B(y)
  case (op1, op2) => op1 * op2
})


// Expression of the CHSH inequality written with correlators
val chsh = Quotient.quotient( A(0)*B(0) + A(0)*B(1) + A(1)*B(0) - A(1)*B(1) )

println(s"We maximize the CHSH expression $chsh")

// Evaluation of monomials: we have trace(f) = trace(f.adjoint) as we can assume all operators/states are real
val L = Quotient.eigenvalueEvaluator(real = true)

// Maximization problem
val problem = L(chsh).maximize

val permuteAB = Free.permutation {
  case A(i) => B(i)
  case B(i) => A(i)
}

val symmetryGroup = Quotient.groupInQuotient(Grp(permuteAB))

val (symmetrizedProblem, _) = problem.symmetrize(evaluationFeasibilityGroup = Some(symmetryGroup))

// Generating set of monomials for the NPA level 1, 2, 3, ...
val level1 = Quotient.quotient(GSet.onePlus(A, B)).pow(1)
val level2 = Quotient.quotient(GSet.onePlus(A, B)).pow(2)
val level3 = Quotient.quotient(GSet.onePlus(A, B)).pow(3)

// Moment-based relaxations

println(s"Level1, resulting relaxation: $relaxation1")
val relaxation1 = symmetrizedProblem.relaxation(level1)
relaxation1.program.recognizeSymmetricGroup.sedumi.writeFile("CHSH_quantum_S2_level1.sedumi.mat")

println(s"Level2, resulting relaxation: $relaxation2")
val relaxation2 = symmetrizedProblem.relaxation(level2)
relaxation2.program.recognizeSymmetricGroup.sedumi.writeFile("CHSH_quantum_S2_level2.sedumi.mat")

println(s"Level3, resulting relaxation: $relaxation3")
val relaxation3 = symmetrizedProblem.relaxation(level3)
relaxation3.program.recognizeSymmetricGroup.sedumi.writeFile("CHSH_quantum_S2_level3.sedumi.mat")
