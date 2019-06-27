/* Example of a form due to Robinson, published in
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
 * 
 */

// Let's import the relevant libraries
interp.repositories() :+= coursier.MavenRepository("https://dl.bintray.com/denisrosset/maven")

@

import $ivy.`net.alasc::symdpoly-core:0.7.6`
import net.alasc.symdpoly._
import defaults._

/* Free monomial monoid in two variables X and Y. We want to work with signed monomials
 * preceded by a sign (-1 or +1), so the cyclotomic order is 2 to include both roots of unity.
 * 
 * SymDPoly supports commutative polynomial rings as quotients of the free noncommutative ring,
 * thus the ceremonial below.
 */
object Free extends free.MonoDef(2) {
  case object X extends HermitianSingleOp
  case object Y extends HermitianSingleOp
  lazy val families = Seq(X, Y)
}

// Import the variables X and Y in the current scope
import Free.{X, Y}

// Monomials are equivalent under commutation of variables, so we define the corresponding
// quotient monoid by the commutation rules.
val Quotient = Free.quotientMonoid(quotient.commutative)

// Polynomial form given in equation (16) page 109, expressed in the commutative quotient polynomial ring.
val f = Quotient.quotient(
  X.pow(6) + Y.pow(6) - X.pow(4)*Y.pow(2)
    - Y.pow(4)*X.pow(2) - X.pow(4) - Y.pow(4)
    - X.pow(2) - Y.pow(2)
    + X.pow(2)*Y.pow(2)*3 + 1
)

println(s"We optimize over the polynomial $f")

// Evaluation of polynomials over real variables
val L = Quotient.eigenvalueEvaluator(real = true)

// Symmetry group that leaves the objective invariant
val group = L(f).invariantSubgroupOf(Quotient.symmetryGroup)

println(s"That polynomial is invariant under a group of order ${group.order} with generators ${group.generators}")

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

val paperGroup = Quotient.groupInQuotientNC(Grp(d, s))
println(s"The paper exhibits a symmetry group of order ${paperGroup.order} with generators ${paperGroup.generators}")

assert(group === paperGroup)

println("We verified that those groups are equal")

/** Optimization problem. */
val problem: Optimization[L.type, Quotient.type] = L(f).minimize

/** Relaxation with all monomials of maximal degree 3. */
val generatingSet = Quotient.quotient(GSet.onePlus(X, Y).pow(3))

println(s"We work with the generating set ${generatingSet} containing the monomials ${generatingSet.toOrderedSet}")

/** Symmetric relaxation. */
val relaxationSym = problem.symmetrize()._1.relaxation(generatingSet)

println(s"The symmetric relaxation: $relaxationSym")

/** Non symmetric relaxation for comparison. */
val relaxationNoSym = problem.relaxation(generatingSet)

println(s"The nonsymmetric relaxation: $relaxationNoSym")

// We write the symmetrized SDP
relaxationSym.program.sdpa.writeFile("robinson_sym.dat-s")
// We write the non symmetrized SDP
relaxationNoSym.program.sdpa.writeFile("robinson_nosym.dat-s")

println(s"We wrote two files: robinson_sym.dat-s and robinson_nosym.dat-s")
