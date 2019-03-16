---
layout: docs
title: "Details: relaxation"
---

# {{page.title}}

We recall that we started with a polynomial defined over a quotient algebra, evaluated according to a set of rules, and finally maximized.
```tut:silent
import net.alasc.symdpoly._
import net.alasc.symdpoly.examples.quantum.CHSH
import CHSH.{Free, Quotient}
import Free.{A, B}
```
```tut
val L = Quotient.evaluator(evaluation.real) // real moment evaluation
val problem = L(CHSH.bellOperator).maximize
```
We now compute an upper bound using a moment-based semidefinite relaxation. For that purpose, we need a set of unique monomials. The level 1 of the NPA hierarchy is readily obtained:

```tut
val level1free = GSet.onePlus(A, B)
level1free.toSortedSet
```
However, at level 2, we make an interesting observation:
```tut
val level2free = GSet.onePlus(A, B).pow(2)
level2free.toSortedSet
```
As the monomials were defined on the *free* algebra, `B(0) B(0)` is part of the monomial list. When passing through the quotient, those are regrouped in equivalence classes:
```tut
val level2 = Quotient.quotient(level2free)
level2.toSortedSet
```

## Relaxation

We create a moment-based relaxation by calling the `relaxation` method on `problem`.

```tut
val relax1 = problem.relaxation(level2)
```

which can then be exported:

```tut
relax1.program.sdpa.writeFile("output.dat-s")
relax1.momentMatrix
```

We can also symmetrize the problem by first discovering the symmetries of the problem.
```tut
val symmetryGroup = L(CHSH.bellOperator).invariantSubgroupOf(Quotient.symmetryGroup)
symmetryGroup.order
```
Then, we define a new optimization problem that forces equivalence between monomials in orbits of the symmetry group.
```tut
val relax2 = problem.forceSymmetrizeNC(symmetryGroup).relaxation(level2)
relax2.momentMatrix
```

The symmetrization can also be done automatically:
```tut
val relax3 = problem.symmetrize().relaxation(level2)
```

## Custom sets of monomials

The generating sets `GSet` can be combined. For example, to define the NPA levels:

```tut
def npaLevel(n: Int) = Quotient.quotient(GSet.onePlus(A, B).pow(n))
npaLevel(1).toSortedSet
npaLevel(2).toSortedSet
npaLevel(3).toSortedSet.size
npaLevel(4).toSortedSet.size
```
and the local levels:
```tut
def localLevel(n: Int) = Quotient.quotient(GSet.onePlus(A).pow(n) * GSet.onePlus(B).pow(n))

localLevel(1).toSortedSet
localLevel(2).toSortedSet
localLevel(3).toSortedSet.size
localLevel(4).toSortedSet.size
```

Finally, we mention the possibility of creating custom monomial lists:
```tut
val myList = Quotient.quotient(GSet.onePlus(A, B) + GSet.word(A, A) + GSet.word(A, A, A) + GSet.word(B, B) + GSet.word(B, B, B))

myList.toSortedSet
```
