package net.alasc.symdpoly


class PPTTest extends CommonSuite {
  import examples.quantum.Sliwa._

  import Free.{A, B, C}
  val m1 = LptA(Quotient.quotient(A(0)*A(1)*B(1)*B(0)*C(0)*C(1)))
  val m2 = LptA(Quotient.quotient(A(0)*A(1)*B(0)*B(1)*C(1)*C(0)))
  assert(m1 == m2)
}
