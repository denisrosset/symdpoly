---
layout: docs
title: Details: evaluation
---

# {{page.title}}

We state that we evaluate polynomials over pure states, but that the resulting coefficients are real.
```tut:silent
import net.alasc.symdpoly._
import net.alasc.symdpoly.examples.CHSH.{Free, Quotient}
import Free.{A, B}

val L = evaluation.pureStateSelfAdjoint(Quotient)
```
