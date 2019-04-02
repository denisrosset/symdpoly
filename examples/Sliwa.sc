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

import $ivy.`net.alasc::symdpoly-core:0.5`
import net.alasc.symdpoly._
import defaults._

// We describe the free variables A(0) A(1) B(0) B(1) C(0) C(1)
object Free extends free.MonoidDef(2) {

  case class A(x: Int) extends HermitianOp
  object A extends HermitianOpFamily1(0 to 1)

  case class B(y: Int) extends HermitianOp
  object B extends HermitianOpFamily1(0 to 1)

  case class C(z: Int) extends HermitianOp
  object C extends HermitianOpFamily1(0 to 1)

  lazy val operators = Seq(A, B, C)
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

val sliwa4 = Quotient.quotient(A(0)*2 + B(0)*C(0) + B(0)*C(1) + B(1)*C(0) - B(1)*C(1)
  - A(0)*B(0)*C(0) - A(0)*B(0)*C(1) - A(0)*B(1)*C(0) + A(0)*B(1)*C(1))

println(s"Optimizing the expression $sliwa4")

/** Evaluator for real states/operators, such that L(f) = L(f.adjoint). */
val L = Quotient.evaluator(evaluation.real)

/** Evaluator for states with positive partial transpose. */
val LptA = Quotient.evaluator(evaluation.partialTransposes(Quotient)(Free.A, Free.B ++ Free.C))
val LptB = Quotient.evaluator(evaluation.partialTransposes(Quotient)(Free.B, Free.A ++ Free.C))
val LptC = Quotient.evaluator(evaluation.partialTransposes(Quotient)(Free.C, Free.A ++ Free.B))
val LptAll = Quotient.evaluator(evaluation.partialTransposes(Quotient)(Free.A, Free.B, Free.C))

val almostQuantumGeneratingSet = Quotient.quotient(GSet.onePlus(A) * GSet.onePlus(B) * GSet.onePlus(C))
val (problem, group) = L(sliwa4).maximize.symmetrize()
println(s"The relaxation without PPT constraints has symmetry of order ${group.order}")
val (problem_ptA, group_ptA) = LptA(sliwa4).maximize.symmetrize()
println(s"The relaxation with PPT constraint A/BC has symmetry of order ${group_ptA.order}")
val (problem_ptB, group_ptB) = LptB(sliwa4).maximize.symmetrize()
println(s"The relaxation with PPT constraint B/AC has symmetry of order ${group_ptB.order}")
val (problem_ptC, group_ptC) = LptC(sliwa4).maximize.symmetrize()
println(s"The relaxation with PPT constraint C/AB has symmetry of order ${group_ptC.order}")
val (problem_ptAll, group_ptAll) = LptAll(sliwa4).maximize.symmetrize()
println(s"The relaxation with PPT constraints A/B/C has symmetry of order ${group_ptAll.order}")

val relaxation = problem.relaxation(almostQuantumGeneratingSet)
val relaxation_ptA = problem_ptA.relaxation(almostQuantumGeneratingSet)
val relaxation_ptB = problem_ptB.relaxation(almostQuantumGeneratingSet)
val relaxation_ptC = problem_ptC.relaxation(almostQuantumGeneratingSet)
val relaxation_ptAll = problem_ptAll.relaxation(almostQuantumGeneratingSet)
println("Writing files in SDPA format.")
relaxation.program.sdpa.writeFile("sliwa4.dat-s")
relaxation_ptA.program.sdpa.writeFile("sliwa4_ptA.dat-s")
relaxation_ptB.program.sdpa.writeFile("sliwa4_ptB.dat-s")
relaxation_ptC.program.sdpa.writeFile("sliwa4_ptC.dat-s")
relaxation_ptAll.program.sdpa.writeFile("sliwa4_ptAll.dat-s")
println("Running SDPA and verifying bound values")
val tol = 1e-6
val qbound = 4*spire.math.sqrt(2.0)-2
val cbound = 2.0
val OptimumFound(Some(pobj), dobj) = relaxation.program.sdpa.solve()
println(s"No PPT constraints: Comparing $pobj to $qbound")
assert((pobj - qbound) < tol)
val OptimumFound(Some(pobj_ptA), dobj_ptA) = relaxation_ptA.program.sdpa.solve()
println(s"PPT constraint A/BC: Comparing $pobj_ptA to $qbound")
assert((pobj_ptA - qbound) < tol)
val OptimumFound(Some(pobj_ptB), dobj_ptB) = relaxation_ptB.program.sdpa.solve()
println(s"PPT constraint B/AC: Comparing $pobj_ptB to $cbound")
assert((pobj_ptB - cbound) < tol)
val OptimumFound(Some(pobj_ptC), dobj_ptC) = relaxation_ptC.program.sdpa.solve()
println(s"PPT constraint C/AB: Comparing $pobj_ptC to $cbound")
assert((pobj_ptC - cbound) < tol)
val OptimumFound(Some(pobj_ptAll), dobj_ptAlll) = relaxation_ptAll.program.sdpa.solve()
println(s"PPT constraint A/B/C: Comparing $pobj_ptAll to $cbound")
assert((pobj_ptAll - cbound) < tol)
