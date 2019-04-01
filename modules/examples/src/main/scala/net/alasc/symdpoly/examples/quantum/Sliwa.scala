package net.alasc.symdpoly
package examples.quantum

import defaults._
import net.alasc.symdpoly.evaluation.Evaluator
import shapeless.Witness


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

  // We now describe the feasibility group, i.e. the group that respects the structure of the quotient monoid

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
  val L = Quotient.evaluator(evaluation.real)

  /** Evaluator for states with positive partial transpose. */
  val LptA = Quotient.evaluator(evaluation.partialTransposes(Quotient)(Free.A, Free.B ++ Free.C))
  val LptB = Quotient.evaluator(evaluation.partialTransposes(Quotient)(Free.B, Free.A ++ Free.C))
  val LptC = Quotient.evaluator(evaluation.partialTransposes(Quotient)(Free.C, Free.A ++ Free.B))
  val LptAll = Quotient.evaluator(evaluation.partialTransposes(Quotient)(Free.A, Free.B, Free.C))

  /** Group that preserves the problem structure. */
  val feasibilityGroup = Quotient.groupInQuotient(Grp(iA, oA0, pT, pC))

  def npaLevel(l: Int): GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A, B, C).pow(l))

  def localLevel(l: Int): GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A).pow(l) * GSet.onePlus(B).pow(l) * GSet.onePlus(C).pow(l))

  val listOfMonomials: Seq[Quotient.MonoType] = for {
    c <- Seq[Free.MonoType](Free.one, C(0), C(1))
    b <- Seq[Free.MonoType](Free.one, B(0), B(1))
    a <- Seq[Free.MonoType](Free.one, A(0), A(1))
  } yield Quotient.quotient(a * b * c)

}

class SliwaInequality(val index0: Int) {
  import SliwaData.{coefficients, bounds}
  import Sliwa.{listOfMonomials, Quotient}
  def index1: Int = index0 + 1
  def expression: Sliwa.Quotient.PolyType =
    (coefficients(index0).tail zip listOfMonomials.tail).foldLeft(Quotient.zero.toPoly) {
      case (acc, (coeff, mono)) => acc + mono * coeff
    }

  def local: Double = bounds(index0, 1)
  def quantum: Double = bounds(index0, 2)
  def almostQuantum: Double = bounds(index0, 3)
  def npaLevel2: Double = bounds(index0, 4)
  def nonSignaling: Double = bounds(index0, 5)
  def almostQuantumTA: Double = bounds(index0, 6)
  def almostQuantumTB: Double = bounds(index0, 7)
  def almostQuantumTC: Double = bounds(index0, 8)
  def almostQuantumTall: Double = bounds(index0, 9)
  def localLevel6TA: Double = bounds(index0, 10)
  def localLevel6TB: Double = bounds(index0, 11)
  def localLevel6TC: Double = bounds(index0, 12)
  def localLevel6Tall: Double = bounds(index0, 13)
}

object SliwaInequality {
  def fromIndex0(index0: Int): SliwaInequality = new SliwaInequality(index0)
  def fromIndex1(index1: Int): SliwaInequality = fromIndex0(index1 - 1)
}

object Sliwa12PPT extends App {
  import Sliwa._

  val generatingSet = localLevel(6)
  val sliwa = SliwaInequality.fromIndex1(12)
  val (problem, _) = LptAll(sliwa.expression).maximize.symmetrize()
  val relaxation = problem.relaxation(generatingSet)
  relaxation.program.sedumi.writeFile("sliwa12_ppt_sedumi.mat")

}
