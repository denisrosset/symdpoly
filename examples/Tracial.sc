/* Example of tracial optimization, taken from

 * S. Burgdorf, I. Klep, and J. Povh, Optimization of Polynomials in Non-Commuting Variables,
 * Springer International Publishing, 2016.
 *
 * (Example 5.13, page 113)
 */

// Let's import the relevant libraries
interp.repositories() :+= coursierapi.MavenRepository.of("https://dl.bintray.com/denisrosset/maven")

@

import $ivy.`net.alasc::symdpoly-core:0.7.6`
import net.alasc.symdpoly._
import defaults._

/** Free monoid containing the operator variables. */
object Free extends free.MonoDef(1) {
  case object X1 extends HermitianSingleOp
  case object X2 extends HermitianSingleOp
  lazy val families = Seq(X1, X2)
}

import Free.{X1, X2}

val f = (X1.pow(2) + X1.pow(3) * 2 + X1.pow(4) * 2 + X1.pow(6) + 3
  - X1.pow(4) * X2 * 4 + X1.pow(4) * X2.pow(2) + X1.pow(3) * X2 * 4 + X1.pow(3) * X2.pow(2) * 2 - X1.pow(3) * X2.pow(3) * 2
  + X1.pow(2) * X2 * 2 - X1.pow(2) * X2.pow(2) + X1*X2*X1*X2 * 8 + X1.pow(2) * X2.pow(3) * 2 - X1*X2 * 4 + X1*X2.pow(2) * 4 + X1 * X2.pow(4) * 6 - X2 * 2
  + X2.pow(2) - X2.pow(3) * 4 + X2.pow(4) * 2 + X2.pow(6) * 2)

println(s"We minimize the polynomial $f")

// Operators are real, so trace(f.adjoint) = trace(f), and we also have the cyclic property trace(f g) = trace(g f)
val L = Free.traceEvaluator(real = true)

// The problem is a minimization problem
val problem = L(f).minimize

// We use the same generating set as NCcycMin would use (degree 3 monmials)
val generatingSet = GSet.onePlus(X1, X2).pow(3)
println(s"The relaxation uses the generating set ${generatingSet} with monomials ${generatingSet.toOrderedSet}")
val relaxation = problem.relaxation(generatingSet)

relaxation.program.sdpa.writeFile("tracial.dat-s")
println("We wrote the file tracial.dat-s")
