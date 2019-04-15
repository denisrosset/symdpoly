package net.alasc.symdpoly

import defaults._

class InflationTest extends CommonSuite {

  test("Bug in Phase") {
    val nCopies = 3

    // arrangement of sources (<>) and parties
    //    <K>  A  <L>  B  <J>  C  <K>
    // the free monoid of all the operators of our problem
    object FM extends free.MonoDef(2) {
      case class A(x: Int, k: Int, l: Int) extends HermitianOp
      object A extends HermitianOpFamily3(0 to 1, 0 until nCopies, 0 until nCopies)

      case class B(y: Int, l: Int, j: Int) extends HermitianOp
      object B extends HermitianOpFamily3(0 to 1, 0 until nCopies, 0 until nCopies)

      case class C(z: Int, j: Int, k: Int) extends HermitianOp
      object C extends HermitianOpFamily3(0 to 1, 0 until nCopies, 0 until nCopies)

      val families = Seq(A, B, C)
    }

    // import the values A,B,C in scope to avoid having to write FM.A(..) everywhere
    import FM.{A, B, C}

    // quotient monoid defined by all the substitution rules of the problem
    val QM = FM.quotientMonoid(quotient.pairs {
      // parties commute
      case (a: A, b: B) => a * b
      case (a: A, c: C) => a * c
      case (b: B, c: C) => b * c

      case (b: B, a: A) => a * b
      case (c: C, a: A) => a * c
      case (c: C, b: B) => b * c

      // operators are projective measurements with +/- 1 eigenvalues, thus square to identity
      case (A(x1,l1,r1), A(x2,l2,r2)) if x1 == x2 && l1 == l2 && r1 == r2 => FM.one
      case (B(y1,l1,r1), B(y2,l2,r2)) if y1 == y2 && l1 == l2 && r1 == r2 => FM.one
      case (C(z1,l1,r1), C(z2,l2,r2)) if z1 == z2 && l1 == l2 && r1 == r2 => FM.one

      // commutation rules of inflation
      case (a1@A(_,l1,r1), a2@A(_,l2,r2)) if l1 > l2 && r1 != r2 => a2 * a1 // they commute
      case (b1@B(_,l1,r1), b2@B(_,l2,r2)) if l1 > l2 && r1 != r2 => b2 * b1 // they commute
      case (c1@C(_,l1,r1), c2@C(_,l2,r2)) if l1 > l2 && r1 != r2 => c2 * c1 // they commute

      case (op1, op2) => op1 * op2
    })

    // Inflation symmetries, which are *always* valid
    // we organize the parties and sources as follows
    //    <K>  A  <L>  B  <J>  C <K>
    def permuteL(pi: Perm): FM.PermutationType = FM.permutation({
      case A(x,k,l) => A(x,k,l <|+| pi)
      case B(y,l,j) => B(y,l <|+| pi,j)
      case op => op
    })

    def permuteK(pi: Perm): FM.PermutationType = FM.permutation({
      case A(x,k,l) => A(x,k <|+| pi,l)
      case C(z,j,k) => C(z,j,k <|+| pi)
      case op => op
    })

    def permuteJ(pi: Perm): FM.PermutationType = FM.permutation({
      case B(y,l,j) => B(y,l,j <|+| pi)
      case C(z,j,k) => C(z,j <|+| pi,k)
      case op => op
    })

    import net.alasc.named.Symmetric

    val grpJ = Grp(Symmetric(nCopies).generators.map(permuteJ): _*)
    val grpK = Grp(Symmetric(nCopies).generators.map(permuteK): _*)
    val grpL = Grp(Symmetric(nCopies).generators.map(permuteL): _*)

    // Symmetry group of the inflation only (minimal symmetry group always present)
    val inflationSymmetries = QM.groupInQuotient(grpJ union grpK union grpL)

    // Possible symmetries, that need to be restricted to the symmetry group of the inequality

    // transpose Alice and Bob, transpose J and K
    val pT = FM.permutation {
      case C(z,j,k) => C(z,k,j) // party is not moved, but sources are
      case A(x,k,l) => B(x,l,k)
      case B(y,l,j) => A(y,j,l)
    }

    // cyclic permutation Alice -> Bob -> Charlie -> Alice, L -> J -> K -> L
    val pC = FM.permutation {
      case A(x,k,l) => B(x,k,l)
      case B(y,l,j) => C(y,l,j)
      case C(z,j,k) => A(z,j,k)
    }

    // flip Alice input
    val iA = FM.permutation {
      case A(x,k,l) => A(1-x,k,l)
      case op => op
    }

    // flip Alice output for x = 0
    val oA0 = FM.permutation {
      case A(x,k,l) if x == 0 => -A(x,k,l)
      case op => op
    }

    // Symmetries of the underlying Bell scenario, i.e. all symmetries except those related to inflation
    val bellSymmetries = QM.groupInQuotient(Grp(pC, pT, oA0, iA))

    // Construct the feasibility group, which correspond to symmetries that preserve the structure of the problem
    // but not necessarily the objective
    val feasibilityGroup = inflationSymmetries union bellSymmetries

    // for the choice of source indices (0,0,0)
    def a(x: Int) = A(x,0,0)
    def b(y: Int) = B(y,0,0)
    def c(z: Int) = C(z,0,0)
    def ab(x: Int, y: Int) = a(x)*b(y)
    def bc(y: Int, z: Int) = b(y)*c(z)
    def ac(x: Int, z: Int) = a(x)*c(z)
    def abc(x: Int, y: Int, z: Int) = a(x)*b(y)*c(z)

    // two levels of the hierarchy
    val localLevel1 = QM.quotient(GSet.onePlus(A)*GSet.onePlus(B)* GSet.onePlus(C))
    val npaLevel2 = QM.quotient(GSet.onePlus(A, B, C).pow(2))

    val L = QM.eigenvalueEvaluator(true)

    def inflationMaximize(expression: QM.PolyType, useExpressionSymmetries: Boolean = true, useInflationSymmetries: Boolean = true): (Optimization[_ <: evaluation.Evaluator.Aux[QM.type] with Singleton, QM.type], Grp[QM.PermutationType]) = {
      cforRange(0 until expression.nTerms) { i =>
        val mono = expression.monomial(i)
        val monoNormalForm = mono.normalForm
        cforRange(0 until monoNormalForm.length) { j =>
          monoNormalForm(j) match {
            case A(x, 0, 0) =>
            case B(y, 0, 0) =>
            case C(z, 0, 0) =>
            case op => sys.error(s"Invalid operator ${op} in monomial $mono, the expression should involve only source indices = 0.")
          }
        }
      }
      val symE = if (useExpressionSymmetries) expression.invariantSubgroupOf(bellSymmetries) else Grp.trivial[QM.PermutationType]
      val symI = if (useInflationSymmetries) inflationSymmetries else Grp.trivial[QM.PermutationType]
      val symGrp = symE union symI
      (L(expression).maximize.forceSymmetrizeNC(symGrp), symGrp)
    }
    // Tested inequality
    val svetlichny = QM.quotient( abc(1,0,0) + abc(0,1,0) + abc(0,0,1) - abc(1,1,1) - abc(0,1,1) - abc(1,0,1) - abc(1,1,0) + abc(0,0,0) )

    val (problem, group) = inflationMaximize(svetlichny)

  }
}
