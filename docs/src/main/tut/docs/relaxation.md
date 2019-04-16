---
layout: docs
title: "Details: relaxation"
---

# {{page.title}}

We recall that we started with a polynomial defined over a quotient algebra, evaluated according to a set of rules, and finally maximized.
```tut:silent
import net.alasc.symdpoly._
import net.alasc.symdpoly.examples.quantum.CHSH
import CHSH.{Free, Quantum}
import Free.{A, B}
```
```tut
val bellOperator = Quantum.quotient(CHSH.chsh)
val L = Quantum.eigenvalueEvaluator(real = true) // real moment evaluation
val problem = L(bellOperator).maximize
```
We now compute an upper bound using a moment-based semidefinite relaxation. For that purpose, we need a set of unique monomials. The level 1 of the NPA hierarchy is readily obtained:

```tut
val level1free = GSet.onePlus(A, B)
level1free.toOrderedSet
```
However, at level 2, we make an interesting observation:
```tut
val level2free = GSet.onePlus(A, B).pow(2)
level2free.toOrderedSet
```
As the monomials were defined on the *free* algebra, `B(0) B(0)` is part of the monomial list. When passing through the quotient, those are regrouped in equivalence classes:
```tut
val level2 = Quantum.quotient(level2free)
level2.toOrderedSet
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
val symmetryGroup = L(bellOperator).invariantSubgroupOf(Quantum.symmetryGroup)
symmetryGroup.order
```
Then, we define a new optimization problem that forces equivalence between monomials in orbits of the symmetry group.
```tut
val relax2 = problem.forceSymmetrizeNC(symmetryGroup).relaxation(level2)
relax2.momentMatrix
```

The symmetrization can also be done automatically:
```tut
val (problem3, discoveredGroup) = problem.symmetrize()
val relax3 = problem3.relaxation(level2)
```

## Custom sets of monomials

The generating sets `GSet` can be combined. For example, to define the NPA levels:

```tut
def npaLevel(n: Int) = Quantum.quotient(GSet.onePlus(A, B).pow(n))
npaLevel(1).toOrderedSet
npaLevel(2).toOrderedSet
npaLevel(3).toOrderedSet.length
npaLevel(4).toOrderedSet.length
```
and the local levels:
```tut
def localLevel(n: Int) = Quantum.quotient(GSet.onePlus(A).pow(n) * GSet.onePlus(B).pow(n))

localLevel(1).toOrderedSet
localLevel(2).toOrderedSet
localLevel(3).toOrderedSet.length
localLevel(4).toOrderedSet.length
```

Finally, we mention the possibility of creating custom monomial lists:
```tut
val myList = Quantum.quotient(GSet.onePlus(A, B) + GSet.word(A, A) + GSet.word(A, A, A) + GSet.word(B, B) + GSet.word(B, B, B))

myList.toOrderedSet
```
