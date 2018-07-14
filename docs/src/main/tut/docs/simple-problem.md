---
layout: docs
title: Simple Problem
---

### {{page.title}}

Before trying the example below, be sure that **SymDPoly** is able to run on your computer, see the [Quick Start guide](quick-start.html)

An evaluation of the Tsirelson's bound for the CHSH inequality can be done in a few lines of code. This corresponds to the Example 1 of [Navascués, Pironio, Antonio Acín NJP 10 7 (2008) 073013](https://doi.org/10.1088/1367-2630/10/7/073013).

First, we define the free algebra for four operators `A(0)`, `A(1)`, `B(0)` and `B(1)`, corresponding to measurements with outcomes `+1` and `-1`. We then import the operator types `A` and `B` in global scope, instead of writing `Free.A`, `Free.B` when using them.
```tut:silent
import net.alasc.symdpoly._; import net.alasc.symdpoly.joptimizer._

object Free extends free.MonoidDef {
  case class A(x: Int) extends HermitianOp
  object A extends HermitianType1(0 to 1)
  case class B(y: Int) extends HermitianOp
  object B extends HermitianType1(0 to 1)
  val operators = Seq(A, B)
}

import Free.{A, B}
```

We define then the quotient algebra using simple substitution rules: those operators square to the identity, and the operators for Alice and Bob commute (`[A(x), B(y)] = 0`).
```tut:silent
val Quotient = quotient.MonoidDef(Free) {
  case (A(x1), A(x2)) if x1 == x2 => Mono.one
  case (B(y1), B(y2)) if y1 == y2 => Mono.one
  case (B(y), A(x))               => A(x) * B(y)
  case (op1, op2)                 => op1 * op2
}
```

We define one generator of the ambient group (for simplicity), and the ambient group itself. The *ambient group* is a symmetry group that is compatible with the quotient algebra (i.e. it contains permutations `g` of the operators such that `g(quotient(monomial)) = quotient(g(monomial))` ).
```tut:silent
val swapParties = Free.generator {
  case A(i) => B(i)
  case B(i) => A(i)
}
val ambientGroup = Free.ambientGroup(swapParties)
```

Then, we write the operator corresponding to the CHSH inequality. Note that we bring the expression to the quotient algebra where all computations are done.

```tut:silent
val chsh = Quotient.quotient(A(0) * B(0) + A(0) * B(1) + A(1) * B(0) - A(1) * B(1))
```

We then define the generating set of monomials (in the quotient again!). We state that we evaluate polynomials over pure states, but that the resulting coefficients are real. We construct the symmetric semidefinite relaxation.
```tut:silent
val generatingSet = Quotient.quotient(GSet.onePlus(A, B))

val L = evaluation.pureStateSelfAdjoint(Quotient)

val relaxation = L(chsh).maximize.symmetricRelaxation(generatingSet, ambientGroup)
```

Finally, we display the resulting moment matrix and solve the semidefinite program.
```tut
relaxation.gramMatrix.momentMatrix

relaxation.jOptimizerInstance.solve()
```
