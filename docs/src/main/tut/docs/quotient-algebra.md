---
layout: docs
title: "Details: quotient algebra"
---

# {{page.title}}

We define then the quotient algebra using simple substitution rules: those operators square to the identity, and the operators for Alice and Bob commute (`[A(x), B(y)] = 0`).
```tut:silent
import net.alasc.symdpoly._
import net.alasc.symdpoly.examples.CHSH.Free
import Free.{A, B}
val Quotient = quotient.MonoidDef(Free) {
  case (A(x1), A(x2)) if x1 == x2 => Mono.one
  case (B(y1), B(y2)) if y1 == y2 => Mono.one
  case (B(y), A(x))               => A(x) * B(y)
  case (op1, op2)                 => op1 * op2
}
```
