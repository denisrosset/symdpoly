package net.alasc.symdpoly.examples

import cyclo.Cyclo
import net.alasc.symdpoly.evaluation.Symmetries
import net.alasc.symdpoly.math.{GenPerm, PhasedInt}
import net.alasc.symdpoly.{AmbientGroup, GSet, Phase, evaluation, free, quotient}
import scalin.immutable.Mat
import scalin.immutable.dense._

object CGLMP3 extends App {
  object Free extends free.MonoidDef(2) {

    case class A(a: Int, x: Int) extends HermitianOp
    object A extends HermitianType2(0 to 2, 0 to 1)

    case class B(b: Int, y: Int) extends HermitianOp
    object B extends HermitianType2(0 to 2,0 to 1)

    val operators = Seq(A, B)
  }

  import Free.{A, B}

  val Quotient = quotient.MonoidDef(Free) {
    case (A(a1, x1), A(a2, x2)) if x1 == x2 && a1 == a2 => A(a1, x1)
    case (A(a1, x1), A(a2, x2)) if x1 == x2 && a1 != a2 => Free.zero
    case (B(b1, y1), B(b2, y2)) if y1 == y2 && b1 == b2 => B(b1, y1)
    case (B(b1, y1), B(b2, y2)) if y1 == y2 && b1 != b2 => Free.zero
    case (b: B, a: A) => a * b
    case (op1, op2) => op1 * op2
  }

  object G extends AmbientGroup[Quotient.type, Free.type] {
    val swapParties = generator {
      case A(c,z) => B(c,z)
      case B(c,z) => A(c,z)
    }
    val inputSwapA = generator {
      case A(a,0) => A(a,1)
      case A(a,1) => A(a,0)
      case op => op
    }
    val outputA0T = generator {
      case A(0,0) => A(1,0)
      case A(1,0) => A(0,0)
      case op => op
    }
    val outputA0C = generator {
      case A(0,0) => A(1,0)
      case A(1,0) => A(2,0)
      case A(2,0) => A(0,0)
      case op => op
    }
    val generators = Seq(outputA0T, outputA0C, swapParties, inputSwapA)
  }

  implicit def intToCyclo(i: Int): Cyclo = Cyclo(i)

  val coeffs = Mat.rowMajor[Cyclo](6,6)(
    -1,0,1,-1,0,1,
    0,1,-1,1,-1,0,
    1,-1,0,0,1,-1,
    -1,1,0,-1,1,0,
    0,-1,1,1,0,-1,
    1,0,-1,0,-1,1
  )

  val cglmp3 = Quotient.quotient((for {
    a <- 0 to 2
    x <- 0 to 1
    b <- 0 to 2
    y <- 0 to 1 if !coeffs(a+x*3, b+y*3).isZero
  } yield A(a,x)*B(b,y)*coeffs(a+x*3, b+y*3)).reduce(_+_))
  println(cglmp3)
  val L = evaluation.pureStateSelfAdjoint(Quotient)
  val evCglmp3 = L(cglmp3)
  println(evCglmp3)
  val S = Symmetries.symmetrySubgroup(evCglmp3, G.grp)
  println(S)
  println(S.order)
  val generatingSet = Quotient.quotient(GSet.onePlus(A)*GSet.onePlus(B))
  val problem = (evCglmp3).maximize

  val relaxation = problem.symmetricRelaxation(generatingSet, G.grp)
  println(relaxation.objectiveVector)
  println(relaxation.phaseMatrixDescription)
  println(relaxation.momentIndexMatrixDescription)
  println(relaxation.gramMatrix.nUniqueMonomials)
  println(relaxation.canonicalMonomialsDescription)
  def matlabImage(g: GenPerm): Array[Int] = {
    Array.tabulate[Int](relaxation.gramMatrix.matrixSize) { pIndex =>
      g.image(PhasedInt(Phase.one, pIndex)) match {
        case PhasedInt(Phase.one, iIndex) => iIndex + 1
        case PhasedInt(Phase.minusOne, iIndex) => -(iIndex + 1)
      }
    }
  }
  println(relaxation.gramMatrix.matrixSymmetries.generators.map(g => matlabImage(g).mkString("["," ","]")))

}
