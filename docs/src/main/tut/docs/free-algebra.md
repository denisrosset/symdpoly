---
layout: docs
title: "Details: free algebra"
---

# {{page.title}}


First, we define the free algebra for four operators `A(0)`, `A(1)`, `B(0)` and `B(1)`, corresponding to measurements with outcomes `+1` and `-1`. We then import the operator types `A` and `B` in global scope, instead of writing `Free.A`, `Free.B` when using them.
```tut:silent
import net.alasc.symdpoly._

object Free extends free.MonoidDef {
  case class A(x: Int) extends HermitianOp
  object A extends HermitianType1(0 to 1)
  case class B(y: Int) extends HermitianOp
  object B extends HermitianType1(0 to 1)
  val operators = Seq(A, B)
}

import Free.{A, B}
```
