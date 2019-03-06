package net.alasc.symdpoly
package examples

import net.alasc.finite.Grp
import net.alasc.perms.default._
import net.alasc.symdpoly.examples.CGLMP3.Free

object Sliwa {

  object Free extends free.MonoidDef(2) {
    case class A(x: Int) extends HermitianOp
    object A extends HermitianOpFamily1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianOpFamily1(0 to 1)

    case class C(z: Int) extends HermitianOp
    object C extends HermitianOpFamily1(0 to 1)

    val operators = Seq(A, B, C)
  }

  import Free.{A, B, C}

  val Quotient = Free.quotientMonoid(quotient.pairs {
    // parties commute
    case (B(y), A(x)) => A(x)*B(y)
    case (C(z), A(x)) => A(x)*C(z)
    case (C(z), B(y)) => B(y)*C(z)
    // operators are projective measurements with +/- 1 eigenvalues, thus square to identity
    case (A(x1), A(x2)) if x1 == x2 => Free.one
    case (B(y1), B(y2)) if y1 == y2 => Free.one
    case (C(z1), C(z2)) if z1 == z2 => Free.one
    case (op1, op2) => op1*op2
  })

  val pT = Free.permutation { // transpose Alice and Bob
    case A(x) => B(x)
    case B(y) => A(y)
    case op => op
  }

  val pC = Free.permutation { // cyclic permutation Alice -> Bob -> Charlie -> Alice
    case A(x) => B(x)
    case B(y) => C(y)
    case C(z) => A(z)
  }

  val iA = Free.permutation { // flip Alice input
    case A(x) => A(1-x)
    case op => op
  }

  val oA0 = Free.permutation { // flip Alice output for x = 0
    case A(x) if x == 0 => -A(x)
    case op => op
  }

  val ambientGroup = Quotient.groupInQuotient(Grp(pT, pC, iA, oA0))

  def AB(x: Int, y: Int): Free.Monomial = A(x)*B(y)
  def BC(y: Int, z: Int): Free.Monomial = B(y)*C(z)
  def AC(x: Int, z: Int): Free.Monomial = A(x)*C(z)
  def ABC(x: Int, y: Int, z: Int): Free.Monomial = A(x)*B(y)*C(z)

  def select(i: Int, j: Int, k: Int)(x: Int, y: Int, z: Int): Free.Polynomial = ((i,j,k): @unchecked) match {
    case (0,0,0) => Poly.one
    case (1,0,0) => A(x)
    case (0,1,0) => B(y)
    case (0,0,1) => C(z)
    case (1,1,0) => AB(x,y)
    case (1,0,1) => AC(x,z)
    case (0,1,1) => BC(y,z)
    case (1,1,1) => ABC(x,y,z)
  }

  def minusOnePow(i: Int): Int = if (i % 2 == 0) 1 else -1

  def p(a: Int, b: Int, c: Int)(x: Int, y: Int, z: Int): Free.Polynomial = {
    val elements = for {
      i <- Seq(0,1)
      j <- Seq(0,1)
      k <- Seq(0,1)
    } yield select(i,j,k)(x,y,z)*minusOnePow(i*a+j*b+k*c)
    elements.reduce(_+_)/8
  }

  def npaLevel(l: Int): GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A, B, C).pow(l))
  def localLevel(l: Int): GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A).pow(l) * GSet.onePlus(B).pow(l) * GSet.onePlus(C).pow(l))

  val L = Quotient.evaluator.real

  val GYNI = Quotient.quotient( p(0,0,0)(0,0,0) + p(1,1,0)(0,1,1) + p(0,1,1)(1,0,1) + p(1,0,1)(1,1,0) )/4
  val Mermin = Quotient.quotient( ABC(1,0,0) + ABC(0,1,0) + ABC(0,0,1) - ABC(1,1,1) )

}
