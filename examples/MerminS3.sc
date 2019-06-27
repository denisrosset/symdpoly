// Computation of the quantum upper bounds of the Sliwa inequality #4
//
// The inequality was described in
// C. Śliwa, Physics Letters A 317, 165 (2003),
// see also https://arxiv.org/abs/quant-ph/0305190
//
// The 46 families were exhaustively studied in
// J. Vallins, A. B. Sainz, and Y.-C. Liang, Phys. Rev. A 95, 022111 (2017).
//
// see https://journals.aps.org/pra/abstract/10.1103/PhysRevA.95.022111
//
// We use the correlator notation

// Let's import the relevant libraries
interp.repositories() :+= coursier.MavenRepository("https://dl.bintray.com/denisrosset/maven")

@

import $ivy.`net.alasc::symdpoly-core:0.7.3`
import net.alasc.symdpoly._
import defaults._

// We describe the free variables A(0) A(1) B(0) B(1) C(0) C(1)
object Free extends free.MonoDef(2) {

  case class A(x: Int) extends HermitianOp
  object A extends HermitianOpFamily1(0 to 1)

  case class B(y: Int) extends HermitianOp
  object B extends HermitianOpFamily1(0 to 1)

  case class C(z: Int) extends HermitianOp
  object C extends HermitianOpFamily1(0 to 1)

  lazy val families = Seq(A, B, C)
}

// We import the symbols A, B, C in the script scope
import Free.{A, B, C}

// We describe the algebra of these operators
val Quotient = Free.quotientMonoid(quotient.pairs {
  // parties commute, we bring operators in the order A < B < C
  case (B(y), A(x)) => A(x) * B(y)
  case (C(z), A(x)) => A(x) * C(z)
  case (C(z), B(y)) => B(y) * C(z)
  // operators are projective measurements with +/- 1 eigenvalues, thus square to identity
  case (A(x1), A(x2)) if x1 == x2 => Free.one
  case (B(y1), B(y2)) if y1 == y2 => Free.one
  case (C(z1), C(z2)) if z1 == z2 => Free.one
  case (op1, op2) => op1 * op2
})

val cyclic = Free.permutation {
  case A(i) => B(i)
  case B(i) => C(i)
  case C(i) => A(i)
}

val transpose = Free.permutation {
  case A(i) => B(i)
  case B(i) => A(i)
  case op => op
}

val mermin = Quotient.quotient( A(1)*B(0)*C(1) + A(0)*B(1)*C(0) + A(0)*B(0)*C(1) - A(1)*B(1)*C(1) )

println(s"Optimizing the expression $mermin")

/** Evaluator for real states/operators, such that L(f) = L(f.adjoint). */
val L = Quotient.eigenvalueEvaluator(real = true)

val level1 = Quotient.quotient(GSet.onePlus(A, B, C))
val level2 = Quotient.quotient(GSet.onePlus(A, B, C).pow(2))
val level3 = Quotient.quotient(GSet.onePlus(A, B, C).pow(3))

val symmetryGroup = Quotient.groupInQuotient( Grp(cyclic, transpose) )
val (problem, _) = L(mermin).maximize.symmetrize(evaluationFeasibilityGroup = Some(symmetryGroup))

val relaxation1 = problem.relaxation(level1)
val relaxation2 = problem.relaxation(level2)
val relaxation3 = problem.relaxation(level3)

println("Writing files in SeDuMi format.")
relaxation1.program.sedumi.writeFile("mermin_S3_level1.mat")
relaxation2.program.sedumi.writeFile("mermin_S3_level2.mat")
relaxation3.program.sedumi.writeFile("mermin_S3_level3.mat")
