---
layout: docs
title: "Details: symmetries"
---

# {{page.title}}

**SymDPoly** supports symmetry groups that are subgroups of the [Generalized symmetric group](https://en.wikipedia.org/wiki/Generalized_symmetric_group), acting by permuting operator variables, possibly changing their phase. As, currently, support for complex formulations is incomplete in **SymDPoly**, so the elements we use correspond to [signed permutations](https://en.wikipedia.org/wiki/Hyperoctahedral_group).

We show below the syntax used to define those generators, which uses Scala [pattern matching](https://docs.scala-lang.org/tour/pattern-matching.html).
```tut:silent
import net.alasc.symdpoly._
import defaults._
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

Note the minus sign in `outputSwapA0`, which multiplies the phase of `A(0)` by `-1`.

## Ambient group

The *ambient group* of a quotient monoid/algebra is a symmetry group whose action is compatible with the equivalence classes: it contains permutations `g` of the operators such that `g(quotient(monomial)) = quotient(g(monomial))`, and then the action of equivalency classes of the quotient is well defined.

The ambient group is defined simply by:
```tut
val ambientGroup = Free.ambientGroup(swapParties, inputSwapA, outputSwapA0)
```

Currently, **SymDPoly** does not preserve metadata about the group, but simply represents it as a finite group (using the [alasc](https://github.com/denisrosset/alasc) computational group theory library). For example, the group order is readily computed, as well as a small set of generators:

```tut
ambientGroup.order
ambientGroup.smallGeneratingSet
```
