---
layout: docs
title: "Details: evaluation"
---

# {{page.title}}

Having defined the quotient algebra, there can be an additional equivalence relation on the monomials *when they are evaluated*. This depends on the variant of the NC hierarchy in use. Let us reuse the CHSH example, and for simplicity, we define `A(x)` and `B(y)` as quotient monomials.

```tut:silent
import net.alasc.symdpoly._
import net.alasc.symdpoly.examples.quantum.CHSH.{Free, Quotient}
def A(x: Int) = Quotient.quotient(Free.A(x))
def B(y: Int) = Quotient.quotient(Free.B(y))
```

## Original NPA hierarchy, and real-valued variant

For the original NPA hierarchy, there are no additional equivalences.
```tut
val L1 = Quotient.evaluator
L1(A(0)*A(1)).toString
L1(A(1)*A(0)).toString
```
However, sometimes we know that real moment matrices are sufficient to express the optimum. Thus, we can state that we evaluate polynomials over pure states, and that `L(x) = L(x.adjoint)`; as `L` is a `*`-homomorphism (i.e. `L(x.adjoint) = L(x).adjoint`), the resulting coefficients will be real.
```tut
val L2 = L1.real
L2(A(0)*A(1)).toString
L2(A(1)*A(0)).toString
L2(A(0)*A(1)*B(0)*B(1)).toString
L2(A(1)*A(0)*B(0)*B(1)).toString
L2(A(0)*A(1)*B(1)*B(0)).toString
L2(A(1)*A(0)*B(1)*B(0)).toString
```

## PPT constraints

We can also require [PPT constraints](https://arxiv.org/abs/1302.1336), i.e. equivalence of monomials under partial transposition. In our framework, this implies real coefficients as well.
	
```tut
val L3 = L2.transpose { case Free.B(_) => true; case _ => false }
L3(A(0)*A(1)*B(0)*B(1)).toString
L3(A(1)*A(0)*B(0)*B(1)).toString
L3(A(0)*A(1)*B(1)*B(0)).toString
L3(A(1)*A(0)*B(1)*B(0)).toString
```

## Trace optimization

It is also possible to use cyclic equivalency, corresponding to [trace optimization of polynomials](https://www.springer.com/gp/book/9783319333366). We do not have currently examples for that hierarchy, but it is supported by **SymDPoly**'s evaluation framework.

```tut
val L4 = Quotient.evaluator.cyclic
L4(A(0)*A(1)*B(0)*B(1)).toString
L4(A(1)*A(0)*B(0)*B(1)).toString
L4(A(0)*A(1)*B(1)*B(0)).toString
L4(A(1)*A(0)*B(1)*B(0)).toString
```

Internally, **SymDPoly** supports arbitrary combinations of partial transposes and partial cyclic shifts. It remains to be seen if those prove to be useful in problem instances.
