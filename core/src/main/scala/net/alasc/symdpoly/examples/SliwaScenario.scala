package net.alasc.symdpoly
package examples

object SliwaScenario {

  object FM extends free.MonoidDef {
    case class A(x: Int) extends HermitianOp
    object A extends HermitianType1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianType1(0 to 1)

    case class C(z: Int) extends HermitianOp
    object C extends HermitianType1(0 to 1)

    val operators = Seq(A, B, C)
  }

  import FM.{A, B, C}

  val QM = quotient.MonoidDef(FM) {
    // parties commute
    case (B(y), A(x)) => A(x)*B(y)
    case (C(z), A(x)) => A(x)*C(z)
    case (C(z), B(y)) => B(y)*C(z)
    // operators are projective measurements with +/- 1 eigenvalues, thus square to identity
    case (A(x1), A(x2)) if x1 == x2 => Mono.one
    case (B(y1), B(y2)) if y1 == y2 => Mono.one
    case (C(z1), C(z2)) if z1 == z2 => Mono.one
    case (op1, op2) => op1*op2
  }

  // transpose Alice and Bob
  val pT = FM.generator {
    case A(x) => B(x)
    case B(y) => A(y)
    case op => op
  }

  // cyclic permutation Alice -> Bob -> Charlie -> Alice
  val pC = FM.generator {
    case A(x) => B(x)
    case B(y) => C(y)
    case C(z) => A(z)
  }

  // flip Alice input
  val iA = FM.generator {
    case A(x) => A(1-x)
    case op => op
  }

  // flip Alice output for x = 0
  val oA0 = FM.generator {
    case A(x) if x == 0 => -A(x)
    case op => op
  }

  val ambientGroup = QM.ambientGroup(pT, pC, iA, oA0)

  def AB(x: Int, y: Int): FM.Monomial = A(x)*B(y)
  def BC(y: Int, z: Int): FM.Monomial = B(y)*C(z)
  def AC(x: Int, z: Int): FM.Monomial = A(x)*C(z)
  def ABC(x: Int, y: Int, z: Int): FM.Monomial = A(x)*B(y)*C(z)

  def select(i: Int, j: Int, k: Int)(x: Int, y: Int, z: Int): FM.Polynomial = ((i,j,k): @unchecked) match {
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

  def p(a: Int, b: Int, c: Int)(x: Int, y: Int, z: Int): FM.Polynomial = {
    val elements = for {
      i <- Seq(0,1)
      j <- Seq(0,1)
      k <- Seq(0,1)
    } yield select(i,j,k)(x,y,z)*minusOnePow(i*a+j*b+k*c)
    elements.reduce(_+_)/8
  }

  def npaLevel(l: Int): GSet[QM.type] = QM.quotient(GSet.onePlus(A, B, C).pow(l))
  def localLevel(l: Int): GSet[QM.type] = QM.quotient(GSet.onePlus(A).pow(l) * GSet.onePlus(B).pow(l) * GSet.onePlus(C).pow(l))

  val L = evaluation.pureStateSelfAdjoint(QM)

}

object GuessYourNeighborInput extends App {

  import SliwaScenario._

  val objective = QM.quotient( p(0,0,0)(0,0,0) + p(1,1,0)(0,1,1) + p(0,1,1)(1,0,1) + p(1,0,1)(1,1,0) )/4
  val problem = L(objective).maximize()
  val relaxation = problem.symmetricRelaxation(localLevel(1), ambientGroup)
  println(relaxation.jOptimizerInstance.solve(1e-12))

}


object Mermin3 extends App {

  import SliwaScenario._

  val objective = QM.quotient( ABC(1,0,0) + ABC(0,1,0) + ABC(0,0,1) - ABC(1,1,1) )
  val problem = L(objective).maximize()
  val relaxation = problem.symmetricRelaxation(localLevel(1), ambientGroup)
  println(relaxation.jOptimizerInstance.solve(1e-9))

}
