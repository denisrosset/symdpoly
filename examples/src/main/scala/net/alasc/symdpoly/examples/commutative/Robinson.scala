package net.alasc.symdpoly
package examples
package commutative

import defaults._
import net.alasc.symdpoly.matlab._

/** Example of a form due to Robinson, published in
  *
  * B. Reznick, Some concrete aspects of Hilbert’s 17th problem, Contemporary Mathematics, Vol. 253,
  * American Mathematical Society, Providence, RI, 2000, pp. 251–272.
  *
  * and studied in
  *
  * Gatermann & Parrilo, Journal of Pure and Applied Algebra 192 (2004) 95 - 128, page 109
  *
  * (equation 16)
  *
  * symmetric under the dihedral group D4 of order 8.
  */
object Robinson {

  /** Free monomial monoid in two variables X and Y, that includes signed monomials
    * preceded by a sign (-1 or +1), so its cyclotomic order is 2. */
  object Free extends free.MonoidDef(2) {
    case object X extends HermitianSingleOp
    case object Y extends HermitianSingleOp
    val operators = Seq(X, Y)
  }

  // Import the variables X and Y in the current scope
  import Free.{X, Y}

  /** Monomials are equivalent under commutation of variables, so we define the corresponding
    * quotient monoid by the commutation rules.
    */
  val Quotient = Free.quotientMonoid(quotient.commutative)

  /** Polynomial form given in equation (16) page 109, expressed in the quotient polynomial ring. */
  val f = Quotient.quotient(
    X.pow(6) + Y.pow(6) - X.pow(4)*Y.pow(2)
      - Y.pow(4)*X.pow(2) - X.pow(4) - Y.pow(4)
      - X.pow(2) - Y.pow(2)
      + X.pow(2)*Y.pow(2)*3 + 1
  )

  /** A minimization problem is expressed by maximizing -f. */
  val obj = -f

  /** Evaluation of polynomials over real variables. */
  val L = Quotient.evaluator(Evaluation.real)

  /** Symmetry group that leaves the objective invariant. */
  val group = L(obj).invariantSubgroupOf(L.symmetryGroup)

  /** First group generator given in the paper, page 109 */
  val d = Free.permutation {
    case X => -Y
    case Y => X
  }

  /** Second group generator given in the paper, page 109 */
  val s = Free.permutation {
    case X => Y
    case Y => X
  }

  /** We verify that we recover the group described in the paper. */
  assert(group === L.groupInEvaluatorNC(Quotient.groupInQuotientNC(Grp(d, s))))

  /** Optimization problem. */
  val problem: Optimization[L.type, Quotient.type] = L(obj).maximize

  /*
  /** Monomial evaluator invariant under the problem symmetry group. */
  val Lsym = L.symmetric(group)

*/
  /** Relaxation with all monomials of maximal degree 3. */
  val generatingSet = Quotient.quotient(GSet.onePlus(X, Y).pow(3))
/*
  /** Symmetric relaxation. */
  val relaxationSym = problem.relaxation(generatingSet)
*/
  /** Non symmetric relaxation for comparison. */
  val relaxationNoSym = problem.relaxation(generatingSet)

}

object RobinsonApp extends App {
  import Robinson._
/*
  // We write the symmetrized SDP
  relaxationSym.sedumiInstance.writeFile("robinson_sedumi.mat")
*/
  // We write the non symmetrized SDP
  //relaxationNoSym.sedumiInstance.writeFile("robinson_sedumi_nosym.mat")
}
