---
layout: docs
title: "Details: relaxation"
---

# {{page.title}}

We recall that we started with a polynomial defined over a quotient algebra, evaluated according to a set of rules, and finally maximized.
```tut:silent
import net.alasc.symdpoly._
import net.alasc.symdpoly.examples.CHSH
import CHSH.{Free, Quotient}
import Free.{A, B}
```
```tut
val problem = CHSH.L(CHSH.bellOperator).maximize
```
We now compute an upper bound using a moment-based semidefinite relaxation. For that purpose, we need a set of unique monomials. The level 1 of the NPA hierarchy is readily obtained:

```tut
val level1free = GSet.onePlus(A, B)
level1free.monomials
```
However, at level 2, we make an interesting observation:
```tut
val level2free = GSet.onePlus(A, B).pow(2)
level2free.monomials
```
As the monomials were defined on the *free* algebra, `B(0) B(0)` is part of the monomial list. When passing through the quotient, those are regrouped in equivalence classes:
```tut
val level2 = Quotient.quotient(level2free)
level2.monomials
```

## Relaxation

We create a moment-based relaxation by calling the `relaxation` method on `problem`.

```tut
val sdp1 = problem.relaxation(level2)
```

which can then be exported:

```tut
sdp1.sdpaInstance.writeFile("output.dat-s")
sdp1.gramMatrix.momentSet.nMonomials
```

We can also pass an ambient group to the `symmetricRelaxation` method, in which case the symmetry subgroup that leaves the objective invariant is automatically computed.

```tut
val sdp2 = problem.symmetricRelaxation(level2, CHSH.ambientGroup)
sdp2.gramMatrix.momentSet.nMonomials
```

## Custom sets of monomials

The generating sets `GSet` can be combined. For example, to define the NPA levels:

```tut
def npaLevel(n: Int) = Quotient.quotient(GSet.onePlus(A, B).pow(n))
npaLevel(1).monomials
npaLevel(2).monomials
npaLevel(3).monomials.size
npaLevel(4).monomials.size
```
and the local levels:
```tut
def localLevel(n: Int) = Quotient.quotient(GSet.onePlus(A).pow(n) * GSet.onePlus(B).pow(n))

localLevel(1).monomials
localLevel(2).monomials
localLevel(3).monomials.size
localLevel(4).monomials.size
```

Finally, we mention the possibility of creating custom monomial lists:
```tut
val myList = Quotient.quotient(GSet.onePlus(A, B) + GSet.word(A, A) + GSet.word(A, A, A) + GSet.word(B, B) + GSet.word(B, B, B))

myList.monomials
```
