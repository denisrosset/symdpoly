---
layout: docs
title: Exporting structured data
position: 3
---

# {{page.title}}

Again, we work with the CHSH relaxation.

```tut
import net.alasc.symdpoly.examples.CHSH
val relaxation = CHSH.relaxation
```

Note that this CHSH example includes all relabelings of parties/inputs/outputs.

```tut
CHSH.swapParties
CHSH.inputSwapA
CHSH.outputSwapA0
CHSH.ambientGroup.order
```

## Exploring the monomial matrix structure

The `momentMatrixDescription` method returns a user-friendly text description of the moment matrix.

```tut
relaxation.momentMatrixDescription
```

Note that the monomials are included into brackets, as they represent equivalence classes of monomials in the quotient algebra. 

This description works only for smaller moment matrices. We now see how to get more compact descriptions.

First, the method `canonicalMonomials` returns a list of the monomials present. The first element of each row is the monomial index, followed by the canonical representative of that monomial under the symmetry group and the quotient algebra rewriting rules. Then, after the colon `:`, the orbit of (quotient) monomials under the symmetry group. The symmetry group can flip the sign of monomials, thus some elements on the right of `:` can be preceded by a phase.
```tut
relaxation.canonicalMonomialsDescription(CHSH.symGroup)
```
The moment matrix is then described using that list. The first line provides the size of the moment matrix, following by the number of unique monomials.
Then, the matrix of monomials indices.
```tut
relaxation.momentIndexMatrixDescription
```
Note that monomial indexing is `0`-based, and that `-1` represents a cell of the moment matrix always equal to `0`.
Those monomials can acquire a phase, either during rewriting or due to the action of the symmetry group. Phases are provided by another matrix:
```tut
relaxation.phaseMatrixDescription
```

## Writing to files

To write any of the above to a file, use:

```tut
scala.tools.nsc.io.File("matrix.txt").writeAll(relaxation.momentMatrixDescription)
```
