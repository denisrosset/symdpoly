package net.alasc.symdpoly.examples

import net.alasc.symdpoly.{GSet, Poly, evaluation, free, quotient}

object Distributed {

  object Free extends free.MonoidDef(2) {

    case class A(x: Int) extends HermitianOp
    object A extends HermitianOpFamily1(0 to 1)

    case class B(y: Int) extends HermitianOp
    object B extends HermitianOpFamily1(0 to 1)

    val operators = Seq(A, B)
  }

  import Free.{A, B}

  val Quotient = Free.quotientMonoid(quotient.pairs {
    case (A(x1), A(x2)) if x1 == x2 => Free.one
    case (B(y1), B(y2)) if y1 == y2 => Free.one
    case (B(y), A(x)) => A(x) * B(y)
/*    case (A(x1), A(x2)) if x1 > x2 => A(x2) * A(x1)
    case (B(y1), B(y2)) if y1 > y2 => B(y2) * B(y1)*/
    case (op1, op2) => op1 * op2
  })

  def PA(a: Int, x: Int) = a match {
    case 0 => (Poly.one[Free.type, Free.type] + A(x).toMono)/2
    case 1 => (Poly.one[Free.type, Free.type]- A(x).toMono)/2
  }

  def PB(b: Int, y: Int) = b match {
    case 0 => (Poly.one[Free.type, Free.type] + B(y).toMono)/2
    case 1 => (Poly.one[Free.type, Free.type] - B(y).toMono)/2
  }

  def PAB(a: Int, b: Int, x: Int, y: Int) = PA(a,x) * PB(b,y)

  val objective = (for {
    a <- 0 to 1
    b <- 0 to 1 if a == b
    x <- 0 to 1
    y <- 0 to 1
  } yield {
    if (x == y) {
      if (a == x) PAB(a,b,x,y) else Poly.zero[Free.type, Free.type]
    } else PAB(a,b,x,y)
  }).reduce(_ + _)


  val bellOperator = Quotient.quotient(objective)

  val generatingSet = Quotient.quotient(GSet.onePlus(A, B).pow(4))

  val L = Quotient.evaluator.real

  val problem = L(bellOperator).maximize

  val feasibilityGrp = Quotient.groupInQuotient(Free.symmetryGroup)

  val symmetryGrp = feasibilityGrp.leavesInvariant(L(bellOperator))

  val Lsym = L.symmetric(symmetryGrp)

  val relaxation = Lsym(bellOperator).maximize.relaxation(generatingSet)

}

object DistributedApp extends App {
  import Distributed._

  relaxation.mosekInstance.writeCBF("distributed.cbf")
}
