---
layout: docs
title: "Details: evaluation"
---

# {{page.title}}

Having defined the quotient algebra, there can be an additional equivalence relation on the monomials *when they are evaluated*. This depends on the variant of the NC hierarchy in use. Let us reuse the CHSH example, and for simplicity, we define `A(x)` and `B(y)` as quotient monomials.

```tut:silent
import net.alasc.symdpoly._
import net.alasc.symdpoly.examples.quantum.CHSH.{Free, Quantum}
def A(x: Int) = Quantum.quotient(Free.A(x))
def B(y: Int) = Quantum.quotient(Free.B(y))
```

## Original NPA hierarchy and real-valued variant

For the original NPA hierarchy, there are no additional equivalences.
```tut
val L1 = Quantum.eigenvalueEvaluator()
L1(A(0)*A(1)).toString
L1(A(1)*A(0)).toString
```
However, sometimes we know that real moment matrices are sufficient to express the optimum. Thus, we can state that we evaluate polynomials over pure states, and that `L(x) = L(x.adjoint)`; as `L` is a `*`-homomorphism (i.e. `L(x.adjoint) = L(x).adjoint`), the resulting coefficients will be real.
```tut
val L2 = Quantum.eigenvalueEvaluator(real = true)
L2(A(0)*A(1)).toString
L2(A(1)*A(0)).toString
L2(A(0)*A(1)*B(0)*B(1)).toString
L2(A(1)*A(0)*B(0)*B(1)).toString
L2(A(0)*A(1)*B(1)*B(0)).toString
L2(A(1)*A(0)*B(1)*B(0)).toString
```

## PPT constraints

We can also require [PPT constraints](https://arxiv.org/abs/1302.1336), i.e. equivalence of monomials under partial transposition.
The user specifies the families of operators that commute.
	
```tut
val L3 = Quantum.pptEvaluator(Free.A, Free.B)
L3(A(0)*A(1)*B(0)*B(1)).toString
L3(A(1)*A(0)*B(0)*B(1)).toString
L3(A(0)*A(1)*B(1)*B(0)).toString
L3(A(1)*A(0)*B(1)*B(0)).toString

import net.alasc.symdpoly.examples.quantum.Sliwa.{Free => SliwaFree, Quotient => SliwaQuotient}

val L3bis = SliwaQuotient.pptEvaluator(SliwaFree.A, SliwaFree.B ++ SliwaFree.C)

```

## Trace optimization

It is also possible to use cyclic equivalency, corresponding to [trace optimization of polynomials](https://www.springer.com/gp/book/9783319333366). 

```tut
val L4 = Free.traceEvaluator(real = true)
L4(Free.A(0)*Free.A(1)*Free.B(0)*Free.B(1)).toString
L4(Free.A(1)*Free.A(0)*Free.B(0)*Free.B(1)).toString
L4(Free.A(0)*Free.A(1)*Free.B(1)*Free.B(0)).toString
L4(Free.A(1)*Free.A(0)*Free.B(1)*Free.B(0)).toString
```

Note: the use of trace optimization with rewriting rules has not yet been proven sound. 
