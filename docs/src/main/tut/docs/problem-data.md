---
layout: docs
title: Exporting structured data
position: 3
---

# {{page.title}}

Again, we work with the CHSH relaxation.

```tut
import net.alasc.symdpoly.examples.quantum.CHSH
val relaxation = CHSH.relaxation
```

## Exploring the monomial matrix structure

To explore the moment matrix, inspect:

```tut
relaxation.momentMatrix
```

Note that the monomials are included into brackets, as they represent equivalence classes of monomials in the quotient algebra. 

This description works only for smaller moment matrices.

The method `allMoments` returns a list of the monomials present. 
```tut
relaxation.allMoments
```
