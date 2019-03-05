package net.alasc.symdpoly
package examples
package commutative

import net.alasc.symdpoly.defaults._
import net.alasc.symdpoly.matlab._

object Choi extends App {

  object Free extends free.MonoidDef(2) {

    case class X(i: Int) extends HermitianOp
    object X extends HermitianOpFamily1(1 to 3)

    case class Y(i: Int) extends HermitianOp
    object Y extends HermitianOpFamily1(1 to 3)
    val operators = Seq(X, Y)

  }

  import Free.{X, Y}

  val Quotient = quotient.MonoidDef.commutative(Free)

  val B = X(1).pow(2)*Y(1).pow(2) + X(2).pow(2)*Y(2).pow(2) + X(3).pow(2)*Y(3).pow(2) -
    (X(1)*X(2)*Y(1)*Y(2) + X(2)*X(3)*Y(2)*Y(3) + X(3)*X(1)*Y(3)*Y(1)) * 2 +
    (X(1).pow(2)*Y(2).pow(2) + X(2).pow(2)*Y(3).pow(2) + X(3).pow(2)*Y(1).pow(2))

  val S = (X(1).pow(2) + X(2).pow(2) + X(3).pow(2) + Y(1).pow(2) + Y(2).pow(2) + Y(3).pow(2))

  val flipX1 = Free.permutation {
    case X(1) => -X(1)
    case op => op
  }

  val cyclic = Free.permutation {
    case X(1) => X(2)
    case X(2) => X(3)
    case X(3) => Y(1)
    case Y(1) => Y(2)
    case Y(2) => Y(3)
    case Y(3) => X(1)
  }

  val swapX1X2 = Free.permutation {
    case X(1) => X(2)
    case X(2) => X(1)
    case op => op
  }

  val generatingSet = Quotient.quotient(GSet.onePlus(X, Y).pow(3))

  val L = Quotient.evaluator.real

  val feasibilityGroup = Quotient.groupInQuotient(Grp(flipX1, cyclic, swapX1X2))

  val obj = Quotient.quotient(-B*S)

  val symmetryGroup = feasibilityGroup.leavesInvariant(L(obj))

  val Lsym = L.symmetric(symmetryGroup)

  val problem = Lsym(obj).maximize

  val relaxationNoSym = L(obj).maximize.relaxation(generatingSet)
  val relaxationSym = Lsym(obj).maximize.relaxation(generatingSet)
  relaxationNoSym.mosekInstance.writeCBF("choinosym.cbf")
  relaxationSym.mosekInstance.writeCBF("choisym.cbf")

  relaxationSym.sedumiInstance.writeFile("choi_sedumi.mat")

}
