package net.alasc.symdpoly
package examples.quantum

import defaults._

/** Facet inequalities in the scenario with three parties and binary inputs/outputs.
  *
  * Described in
  *
  * C. Śliwa, Physics Letters A 317, 165 (2003), see also https://arxiv.org/abs/quant-ph/0305190
  *
  */
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
    case (B(y), A(x)) => A(x) * B(y)
    case (C(z), A(x)) => A(x) * C(z)
    case (C(z), B(y)) => B(y) * C(z)
    // operators are projective measurements with +/- 1 eigenvalues, thus square to identity
    case (A(x1), A(x2)) if x1 == x2 => Free.one
    case (B(y1), B(y2)) if y1 == y2 => Free.one
    case (C(z1), C(z2)) if z1 == z2 => Free.one
    case (op1, op2) => op1 * op2
  })

  /** Transposition of Alice and Bob. */
  val pT = Free.permutation {
    case A(x) => B(x)
    case B(y) => A(y)
    case op => op
  }

  /** Cyclic permutation Alice -> Bob -> Charlie -> Alice. */
  val pC = Free.permutation {
    case A(x) => B(x)
    case B(y) => C(y)
    case C(z) => A(z)
  }

  /** Flip of Alice's input. */
  val iA = Free.permutation {
    case A(x) => A(1 - x)
    case op => op
  }

  /** Flip Alice output for x = 0. */
  val oA0 = Free.permutation {
    case A(x) if x == 0 => -A(x)
    case op => op
  }


  /** Default evaluator. */
  val L = Quotient.evaluator(Evaluation.real)

  /** Group that preserves the problem structure. */
  val feasibilityGroup = L.groupInEvaluator(Quotient.groupInQuotient(Grp(pT, pC, iA, oA0)))

  def npaLevel(l: Int): GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A, B, C).pow(l))

  def localLevel(l: Int): GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A).pow(l) * GSet.onePlus(B).pow(l) * GSet.onePlus(C).pow(l))

  val listOfMonomials: Seq[Quotient.Monomial] = for {
    a <- Seq[Free.Monomial](Free.one, A(0), A(1))
    b <- Seq[Free.Monomial](Free.one, B(0), B(1))
    c <- Seq[Free.Monomial](Free.one, C(0), C(1))
  } yield Quotient.quotient(a * b * c)

  val expressions = Seq( // coefficients of the 46 inequalities
    Seq(1, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Seq(2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, -1, 0, 0, 0, 0, 0, -1, 0, 1, 0),
    Seq(2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1),
    Seq(2, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1, 0),
    Seq(3, -1, 0, -1, 0, -1, 0, -1, 1, -1, 0, -1, 0, 1, 1, -1, 1, 0, 0, -1, 1, -1, 1, 0, 1, 0, -1),
    Seq(3, -1, 0, -1, -1, 0, 0, 0, 0, -1, 0, -1, 0, 1, 1, -1, 1, 0, 0, -1, 1, 1, 0, -1, -1, 1, 0),
    Seq(4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1),
    Seq(4, 0, 0, 0, -1, -1, 0, -1, -1, 0, 0, 0, 0, -2, 0, 0, 0, 2, 0, 0, 0, 0, -1, 1, 0, 1, -1),
    Seq(4, 0, 0, 0, -1, -1, 0, -1, -1, 0, 0, 0, 0, -2, 0, 0, 2, 0, 0, 0, 0, 0, -1, 1, 0, -1, 1),
    Seq(4, 0, 0, 0, -1, -1, 0, -1, -1, 0, -1, 1, -1, -1, 0, 1, 0, 1, 0, -1, 1, 1, 0, -1, -1, 1, 0),
    Seq(4, 0, 0, 0, -2, 0, 0, 0, -2, 0, 0, 0, 0, -1, -1, 0, 1, 1, 0, 0, 0, 0, -1, 1, 0, -1, 1),
    Seq(4, 0, 0, 0, -2, 0, 0, 0, -2, 0, -1, -1, 1, 0, -1, 1, 1, 0, 0, -1, -1, 1, 0, 1, 1, -1, 0),
    Seq(4, 0, 0, 0, -2, -2, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1),
    Seq(4, 0, 0, 0, -2, -2, 0, 0, 0, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, -1, 1, 0, 0, 0, 0, 1, -1),
    Seq(4, 0, 0, 0, -2, -2, 0, 0, 0, 0, -1, -1, 2, 0, 0, 0, -1, 1, 0, -1, -1, 2, 0, 0, 0, 1, -1),
    Seq(4, -1, -1, 0, -1, -1, 0, 0, 0, 0, -1, -1, 0, 0, 2, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1),
    Seq(4, -1, -1, 0, -1, -1, 0, 0, 0, 0, -1, -1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2, 2),
    Seq(4, -1, -1, 0, -1, -1, 0, 0, 0, 0, -1, -1, 2, 0, 0, 0, -1, 1, 0, 0, 0, 0, -1, 1, -2, 1, 1),
    Seq(4, -1, -1, 0, -1, -1, 0, 0, 0, 0, -1, -1, 2, 0, 0, -2, 1, 1, 0, 0, 0, 0, -1, 1, 0, -1, 1),
    Seq(4, -1, -1, 0, -1, 1, 0, -1, 1, 0, -1, 1, 1, -1, -1, 1, -1, -1, 0, 0, 0, -1, 1, 1, 1, -1, -1),
    Seq(4, -1, -1, -1, -1, 0, -1, 0, 1, 0, -1, -1, -1, 2, 1, -1, 1, 0, 0, 0, 0, 0, -1, 1, 0, 1, -1),
    Seq(4, -1, -1, -1, -1, 0, -1, 0, 1, -1, -1, 0, -1, 2, 1, 0, 1, -1, -1, 0, 1, 0, 1, -1, 1, -1, 0),
    Seq(4, -1, -1, -1, 1, 1, -1, 1, 1, 0, -1, 1, 0, 1, -1, 0, 1, -1, 0, 0, 0, -1, 1, 1, 1, -1, -1),
    Seq(5, -1, 0, -1, 0, -1, 0, -1, -1, -1, 0, -1, 1, -2, 1, 0, 0, 2, 0, -1, -1, 0, 0, 2, 0, 1, -1),
    Seq(5, -1, 0, -1, 0, -1, 0, -1, -1, -1, 0, -1, 1, -2, 1, 0, 0, 2, 0, -1, -1, 0, 2, 0, 0, -1, 1),
    Seq(5, -1, 0, -1, -1, 0, 0, 0, -2, -1, -1, 0, -1, 1, 0, 0, 0, 2, 0, 0, -2, 0, 0, 2, 2, -2, 0),
    Seq(5, -2, -1, -1, 1, 0, 0, -1, -1, -1, 1, 0, 0, -2, 2, -1, 1, 0, 0, -1, -1, -1, 1, 0, -1, 2, 1),
    Seq(6, -1, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 1, -2, -1, -1, 1, 2, 0, 0, 0, -1, 1, 2, -1, 3, 0),
    Seq(6, -1, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 1, -2, -1, -1, 1, 2, 0, 0, 0, -1, 3, 0, -1, 1, 2),
    Seq(6, -1, -1, 0, -2, 2, 0, -1, 1, 0, -1, 1, 1, -2, -1, 1, -1, -2, 0, 0, 0, -1, 2, 1, 1, -2, -1),
    Seq(6, -1, -1, -1, 0, 1, -1, 1, 0, 0, -1, 1, 0, 0, -2, 0, 1, -3, 0, 0, 0, -1, 2, 1, 1, -2, -1),
    Seq(6, -1, -1, -1, 0, 1, -1, 1, 0, 0, -2, 2, 0, 0, -2, 0, 0, -2, 0, -1, 1, 1, -2, -1, -1, 1, 2),
    Seq(6, -1, -1, -1, 0, 1, -1, 1, 0, -1, 0, 1, 0, 0, -2, 1, -2, -1, -1, 1, 0, 1, -2, -1, 0, -1, 3),
    Seq(6, -1, -1, -1, 0, 1, -1, 1, 0, -1, 0, 1, 1, 2, -1, 2, -2, -2, -1, 1, 0, 2, 0, 0, 1, -1, 2),
    Seq(6, -1, -1, -1, 1, 2, -1, 2, 1, 0, -1, 1, 0, 1, -1, 0, 2, -2, 0, 0, 0, -1, 2, 1, 1, -2, -1),
    Seq(6, -2, 0, 0, -1, -1, 0, -1, -1, 0, -1, -1, -1, 2, -1, 1, -1, 2, 0, -1, -1, 1, -1, 2, 1, -2, 1),
    Seq(6, -2, 0, 0, -1, -1, 0, -1, -1, 0, -1, -1, -1, 3, 0, 1, -2, 1, 0, -1, -1, 1, -2, 1, 1, -1, 2),
    Seq(6, -2, 0, 0, -2, -2, 0, 0, 0, 0, -1, -1, 1, -1, 2, -1, 2, -1, 0, -1, -1, 1, -1, 2, 1, -2, 1),
    Seq(6, -2, 0, -2, 1, -1, 0, -1, -1, -2, 1, -1, 1, -2, 1, -1, 1, 2, 0, -1, -1, -1, 1, 2, -1, 2, -1),
    Seq(6, -2, -2, -2, 1, 1, 0, -1, -1, 0, -1, -1, -2, 1, 1, -2, 2, 2, 0, -1, 1, 0, 2, -2, 0, -1, 1),
    Seq(7, -1, 0, -1, -1, 0, 0, 0, 0, -1, 0, -1, 0, 3, 1, -1, 1, 2, 0, -1, 1, -1, 4, -1, 1, -1, -2),
    Seq(8, -1, -1, -1, -1, 0, -1, 0, 1, 0, -1, 1, -1, 2, 1, 1, 1, -4, 0, 0, -2, 0, 1, 3, -2, 3, 1),
    Seq(8, -2, 0, -2, 1, -1, 0, -1, 1, 0, -1, -1, -1, 2, 3, 1, -1, -2, 0, -1, 1, -1, 3, 0, -1, 4, -1),
    Seq(8, -2, -2, 0, -2, 2, 0, 0, 0, 0, -1, 1, 2, -2, -2, -2, 1, 3, 0, -1, 1, 2, -2, -2, 2, -3, -1),
    Seq(8, -3, -1, 0, -2, 2, 0, -1, 1, 0, -2, 2, 2, -2, -2, 2, -2, -2, 0, -1, 1, 2, -2, -2, -2, 3, 1),
    Seq(10, -3, -1, -3, 2, 1, -1, 1, 2, 0, -2, 2, -1, 3, -4, -1, 1, -2, 0, -1, -1, -2, 3, 1, 2, -4, -2)
  )

}

object SliwaApp extends App {

  import Sliwa._

  val generatingSet = localLevel(1)

  expressions.zipWithIndex.foreach { case (coefficients, index0) =>
    val index1 = index0 + 1
    println(s"Working on inequality #$index1")
    val expression = (coefficients zip listOfMonomials).foldLeft(Quotient.polyAssociativeAlgebra.zero) {
      case (acc, (coeff, mono)) => acc + mono * coeff
    }
    println(s"Expression: $expression")
    val obj = -expression
    val symmetryGroup = L(obj).invariantSubgroupOf(feasibilityGroup)
    println(s"Symmetry group order: ${symmetryGroup.order}")
    /*
    val Lsym = L.symmetric(symmetryGroup)
    val problem = Lsym(obj).maximize
    val relaxation = problem.relaxation(generatingSet)
    println(s"Number of unique monomials: ${relaxation.gramMatrix.nUniqueMonomials}")
    relaxation.mosekInstance.writeCBF(s"sliwa_$index1.cbf")
     */
  }

}
