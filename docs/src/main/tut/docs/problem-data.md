---
layout: docs
title: Exporting structured data
position: 3
---

# {{page.title}}

Again, we work with the CHSH relaxation.

```tut
import net.alasc.symdpoly._
import net.alasc.symdpoly.examples.quantum.CHSH
import CHSH._
val L = Quantum.eigenvalueEvaluator(real = true)
val generatingSet = Quantum.quotient(GSet.onePlus(Free.A, Free.B))
val relaxation = L(Quantum.quotient(CHSH.chsh)).maximize.relaxation(generatingSet)
```

## Exploring the monomial matrix structure

To explore the moment matrix, inspect:

```tut
relaxation.momentMatrix.mat
```

Note that the monomials are included into brackets, as they represent equivalence classes of monomials in the quotient algebra. 

This description works only for smaller moment matrices.

The method `allMoments` returns a list of the monomials present. 
```tut
relaxation.allMoments
```
