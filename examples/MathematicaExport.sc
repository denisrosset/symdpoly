// Export of data in the Mathematica syntax
//
// Based on CHSH.sc

// Let's import the relevant libraries
interp.repositories() :+= coursier.MavenRepository("https://dl.bintray.com/denisrosset/maven")

@

import $ivy.`net.alasc::symdpoly-core:0.7.1-SNAPSHOT`
import net.alasc.symdpoly._
import defaults._

object Free extends free.MonoDef(2) {
  case class A(x: Int) extends HermitianOp
  object A extends HermitianOpFamily1(0 to 1)
  case class B(y: Int) extends HermitianOp
  object B extends HermitianOpFamily1(0 to 1)
  lazy val families = Seq(A, B)
}

import Free.{A, B}

implicit val printOp = pretty.Mathematica.makePrinter[Free.Op] { case A(x) => s"A$x"; case B(y) => s"B$y" }

val Quantum = Free.quotientMonoid(quotient.pairs {
  case (A(x1), A(x2)) if x1 == x2 => Free.one
  case (B(y1), B(y2)) if y1 == y2 => Free.one
  case (B(y), A(x)) => A(x) * B(y)
  case (op1, op2) => op1 * op2
})

val chsh = Quantum.quotient( A(0)*B(0) + A(0)*B(1) + A(1)*B(0) - A(1)*B(1) )

val L = Quantum.eigenvalueEvaluator(real = true)

val generatingSet = Quantum.quotient(GSet.onePlus(A, B)).pow(2)

val momentMatrix = MomentMatrix[L.type, Quantum.type](generatingSet.toOrderedSet)

momentMatrix.mat.prettyPrint(pretty.Mathematica)
