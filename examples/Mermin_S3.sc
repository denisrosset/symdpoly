// Computation of the quantum upper bounds of the Mermin inequality for three parties
//
// We purposely only exploit the symmetry group S3 that permutes the parties and export
// in SeDuMi format; the "recognizeSymmetricGroup" forces the presentation of S3 in the
// exported files.
//
// The inequality was described in
// C. Śliwa, Physics Letters A 317, 165 (2003),
//
// The 46 families were exhaustively studied in
// J. Vallins, A. B. Sainz, and Y.-C. Liang, Phys. Rev. A 95, 022111 (2017).
//
// see https://journals.aps.org/pra/abstract/10.1103/PhysRevA.95.022111
//
// We use the correlator notation

// Let's import the relevant libraries
interp.repositories() :+= coursierapi.MavenRepository.of("https://dl.bintray.com/denisrosset/maven")

@

import $ivy.`net.alasc::symdpoly-core:0.7.6`
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

val mermin = Quotient.quotient( A(1)*B(0)*C(0) + A(0)*B(1)*C(0) + A(0)*B(0)*C(1) - A(1)*B(1)*C(1) )

println(s"Optimizing the expression $mermin")

/** Evaluator for real states/operators, such that L(f) = L(f.adjoint). */
val L = Quotient.eigenvalueEvaluator(real = true)

val local1 = Quotient.quotient(GSet.onePlus(A) * GSet.onePlus(B) * GSet.onePlus(C))
val local2 = Quotient.quotient(GSet.onePlus(A).pow(2) * GSet.onePlus(B).pow(2) * GSet.onePlus(C).pow(2))
val npa2 = Quotient.quotient(GSet.onePlus(A, B, C).pow(2))
val npa3 = Quotient.quotient(GSet.onePlus(A, B, C).pow(3))

val symmetryGroup = Quotient.groupInQuotient( Grp(cyclic, transpose) )
val (problem, _) = L(mermin).maximize.symmetrize(evaluationFeasibilityGroup = Some(symmetryGroup))

val relaxation_local1 = problem.relaxation(local1)
val relaxation_local2 = problem.relaxation(local2)
val relaxation_npa2 = problem.relaxation(npa2)
val relaxation_npa3 = problem.relaxation(npa3)

println("Writing files in SeDuMi format.")
relaxation_local1.program.recognizeSymmetricGroup.sedumi.writeFile("Mermin_S3_local1.mat")
relaxation_local2.program.recognizeSymmetricGroup.sedumi.writeFile("Mermin_S3_local2.mat")
relaxation_npa2.program.recognizeSymmetricGroup.sedumi.writeFile("Mermin_S3_npa2.mat")
relaxation_npa3.program.recognizeSymmetricGroup.sedumi.writeFile("Mermin_S3_npa3.mat")
