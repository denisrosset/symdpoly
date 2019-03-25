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
import net.alasc.symdpoly.examples.quantum.CHSH.{Free, Quotient}
import Free.{A, B}
```

```tut
val swapParties = Free.permutation {
  case A(i) => B(i)
  case B(i) => A(i)
}

val inputSwapA = Free.permutation {
  case A(0) => A(1)
  case A(1) => A(0)
  case op => op
}

val outputSwapA0 = Free.permutation {
  case A(0) => -A(0)
  case op => op
}
```

Note the minus sign in `outputSwapA0`, which multiplies the phase of `A(0)` by `-1`: and beware that the allowed phases are the roots of unity with a denominator dividing the `cyclotomicOrder` parameter of the free monoid.

## Feasibility group

The *feasibility group* of a quotient monoid/algebra is a symmetry group whose action is compatible with the equivalence classes: it contains permutations `g` of the operators such that `g(quotient(monomial)) = quotient(g(monomial))`, and then the action of equivalency classes of the quotient is well defined.

The feasibility group can be defined from a permutation group on the free monoid. When using `groupInQuotient`, we check compatibility of the generators with the quotient structure: if some generators are incompatible, we look for the subgroup that preserves the quotient structure. The `groupInQuotientNC` method is unsafe and disables this check: you can use it when performance becomes critical and you have already proved that the given group preserves the quotient structure.

Note that the feasibility group can automatically be computed by calling `Quotient.symmetryGroup`.
```tut
val feasibilityGroup = Quotient.groupInQuotient(Grp(swapParties, inputSwapA, outputSwapA0))
val feasibilityGroup1 = Quotient.groupInQuotientNC(Grp(swapParties, inputSwapA, outputSwapA0))
val feasibilityGroup2 = Quotient.symmetryGroup
```

Using the algorithms of [Alasc](https://github.com/denisrosset/alasc), the group order is readily computed, as well as a small set of generators:

```tut
feasibilityGroup.order
feasibilityGroup1.order
feasibilityGroup2.order
feasibilityGroup.smallGeneratingSet
```
