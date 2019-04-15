package net.alasc.symdpoly
package examples
package commutative

import net.alasc.symdpoly.defaults._
import net.alasc.symdpoly.solvers.matlab._

/** Example of a form published by Choi and Lam in
  *
  * M.D. Choi, T.Y. Lam, Extremal positive semidefinite forms, Math. Ann. 231 (1977) 1–18
  *
  * and studied in
  *
  * Gatermann & Parrilo, Journal of Pure and Applied Algebra 192 (2004) 95 - 128, page 109
  *
  * (equation 20)
  *
  * symmetric under a group of order 96.
  */
object Choi {

  /** Free monomial monoid in variables X(1), X(2), X(3), Y(1), Y(2), Y(3)
    * preceded by a sign (-1 or +1), so its cyclotomic order is 2. */
  object Free extends free.MonoDef(2) {

    case class X(i: Int) extends HermitianOp
    object X extends HermitianOpFamily1(1 to 3)

    case class Y(i: Int) extends HermitianOp
    object Y extends HermitianOpFamily1(1 to 3)

    lazy val operators = Seq(X, Y)

  }

  import Free.{X, Y}

  /** Monomials are equivalent under commutation of variables. */
  val Quotient = Free.quotientMonoid(quotient.commutative)

  /** Given form B(X, Y). */
  val B = X(1).pow(2)*Y(1).pow(2) + X(2).pow(2)*Y(2).pow(2) + X(3).pow(2)*Y(3).pow(2) -
    (X(1)*X(2)*Y(1)*Y(2) + X(2)*X(3)*Y(2)*Y(3) + X(3)*X(1)*Y(3)*Y(1)) * 2 +
    (X(1).pow(2)*Y(2).pow(2) + X(2).pow(2)*Y(3).pow(2) + X(3).pow(2)*Y(1).pow(2))

  /** Additional factor to construct a SOS decomposition. */
  val S = (X(1).pow(2) + X(2).pow(2) + X(3).pow(2) + Y(1).pow(2) + Y(2).pow(2) + Y(3).pow(2))

  /** A minimization problem of f is expressed by maximizing -f. */
  val obj = Quotient.quotient(-B*S)

  /** Default evaluator. */
  val L = Quotient.eigenvalueEvaluator(true)

  /** Generating set of monomials. */
  val generatingSet = Quotient.quotient(GSet.onePlus(X, Y).pow(3))

  /** Nonsymmetric relaxation. */
  val relaxation = L(obj).maximize.relaxation(generatingSet)

  /** Symmetric relaxation. */
  val (problemSym, _) = L(obj).maximize.symmetrize()
  val relaxationSym = problemSym.relaxation(generatingSet)

}

object ChoiApp extends App {

  import Choi._

  relaxation.program.mosek.writeFile("choi_nosym.cbf")

  relaxationSym.program.mosek.writeFile("choi_sym.cbf")

  relaxationSym.program.sedumi.writeFile("choi_sym_sedumi.mat")

}
