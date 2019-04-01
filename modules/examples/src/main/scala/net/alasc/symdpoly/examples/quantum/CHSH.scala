package net.alasc.symdpoly
package examples.quantum

import net.alasc.symdpoly.defaults._
import net.alasc.symdpoly.evaluation.Evaluator

/** Computes the Tsirelson bound on the CHSH inequality, written using
  * correlators A(x) and B(y).
  */
object CHSH {

  /** Free monoid containing the operator variables. */
  object Free extends free.MonoidDef(2) {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianOpFamily1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianOpFamily1(0 to 1)

    val operators = Seq(A, B)
  }

  import Free.{A, B}

  /** Quotient monoid, with the following rules:
    *
    * - A(x) and B(y) commute
    * - A(x)*A(x) = B(y)*B(y) = 1
    */
  val Quantum = Free.quotientMonoid(quotient.pairs {
    case (A(x1), A(x2)) if x1 == x2 => Free.one
    case (B(y1), B(y2)) if y1 == y2 => Free.one
    case (B(y), A(x)) => A(x) * B(y)
    case (op1, op2) => op1 * op2
  })

  /** Quotient monoid, with the following rules:
    *
    * - all variables commute
    * - A(x)*A(x) = B(y)*B(y) = 1
    */
  val Classical = Free.quotientMonoid(
    quotient.commutative,
    quotient.rules(A(0)*A(0) -> Free.one, A(1)*A(1) -> Free.one, B(0)*B(0) -> Free.one, B(1)*B(1) -> Free.one)
  )

  /** CHSH expression. */
  val chsh = A(0)*B(0) + A(0)*B(1) + A(1)*B(0) - A(1)*B(1)

}
