---
layout: docs
title: Details: symmetries
---

# {{page.title}}

We define one generator of the ambient group (for simplicity), and the ambient group itself. The *ambient group* is a symmetry group that is compatible with the quotient algebra (i.e. it contains permutations `g` of the operators such that `g(quotient(monomial)) = quotient(g(monomial))` ).
```tut:silent
import net.alasc.symdpoly._
import net.alasc.symdpoly.examples.CHSH.Free
import Free.{A, B}
```

```tut
val swapParties = Free.generator {
  case A(i) => B(i)
  case B(i) => A(i)
}

val inputSwapA = Free.generator {
  case A(0) => A(1)
  case A(1) => A(0)
  case op => op
}

val outputSwapA0 = Free.generator {
  case A(0) => -A(0)
  case op => op
}
```
