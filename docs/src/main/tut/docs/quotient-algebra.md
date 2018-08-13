---
layout: docs
title: "Details: quotient algebra"
---

# {{page.title}}

Many equivalence relations can be handled directly at the level of monomials. Thus, the most efficient way of representing equality constraints is to provide a confluent rewriting system over a free monoid. These are the simplified equivalent of [Groebner bases](https://en.wikipedia.org/wiki/Gr%C3%B6bner_basis) for polynomials. While the rewriting system provided by the user has to be confluent for **SymDPoly** to work correctly, we do not verify that property; accordingly, we do not require the user to specify the monomial ordering by which the confluence property can be verified. However, we require that this ordering is graded: substitutions cannot increase the degree of monomials.

As rules with a left hand side of degree one can be handled by variable substitution at the level of the free monoid, the simplest rules for quotient monoids have degree two. Interestingly, most problems in quantum information only require those simplest rules.

## CHSH
For the CHSH example involving measurements with outcomes `+1`, `-1`, we have the following rules: the operators square to the identity, and the operators for different parties commute. This translates to:

```tut:silent
import net.alasc.symdpoly._
import defaults._
import net.alasc.symdpoly.examples.CHSH.Free
val Quotient = quotient.MonoidDef(Free) {
  case (Free.A(x1), Free.A(x2)) if x1 == x2 => Mono.one
  case (Free.B(y1), Free.B(y2)) if y1 == y2 => Mono.one
  case (Free.B(y),  Free.A(x))              => Free.A(x) * Free.B(y)
  case (op1, op2)                           => op1 * op2
}
```

## Collins-Gisin notation

Now, if we have more than two outcomes, we write using the Collins-Gisin basis:
```tut:silent
val nOutputs = 3
val nInputs = 2
object FreeCG extends free.MonoidDef {
  case class A(a: Int, x: Int) extends HermitianOp
  object A extends HermitianType2(0 to nOutputs - 1, 0 to nInputs - 1)
  case class B(a: Int, x: Int) extends HermitianOp
  object B extends HermitianType2(0 to nOutputs - 1, 0 to nInputs - 1)
  val operators = Seq(A, B)
}
val QuotientCG = quotient.MonoidDef(FreeCG) {
  case (FreeCG.A(a1, x1), FreeCG.A(a2, x2)) if x1 == x2 && a1 == a2 => FreeCG.A(a1, x1)
  case (FreeCG.A(a1, x1), FreeCG.A(a2, x2)) if x1 == x2 && a1 != a2 => Mono.zero
  case (FreeCG.B(b1, y1), FreeCG.B(b2, y2)) if y1 == y2 && b1 == b2 => FreeCG.B(b1, y1)
  case (FreeCG.B(b1, y1), FreeCG.B(b2, y2)) if y1 == y2 && b1 != b2 => Mono.zero
  case (FreeCG.B(b, y), FreeCG.A(a, x))                             => FreeCG.A(a, x) * FreeCG.B(b, y)
  case (op1, op2)                                                   => op1 * op2
}
```
```tut
QuotientCG.quotient(FreeCG.A(0, 1) * FreeCG.A(1, 1))
QuotientCG.quotient(FreeCG.A(0, 1) * FreeCG.A(0, 1))
```

Then, polynomials over those operators can be defined using the helper functions:
```tut:silent
def allA(x: Int) = for(a <- 0 to nOutputs - 2) yield (QuotientCG.quotient(FreeCG.A(a, x)): QuotientCG.Polynomial)
def A(a: Int, x: Int): QuotientCG.Polynomial = if (a == nOutputs - 1) Poly.constant[QuotientCG.type, FreeCG.type](Cyclo.one) - allA(x).reduce(_ + _) else QuotientCG.quotient(FreeCG.A(a, x))
def allB(y: Int) = for(b <- 0 to nOutputs - 2) yield (QuotientCG.quotient(FreeCG.B(b, y)): QuotientCG.Polynomial)
def B(b: Int, y: Int): QuotientCG.Polynomial = if (b == nOutputs - 1) Poly.constant[QuotientCG.type, FreeCG.type](Cyclo.one) - allB(y).reduce(_ + _) else QuotientCG.quotient(FreeCG.B(b, y))
```

```tut
A(0, 1)
A(2, 1)
A(2, 1) * B(2, 1)
```


## Pauli matrices

Finally, we show an example where the rewrite rule includes a phase.
```tut:silent
object PauliFree extends free.MonoidDef {
  case class σ(i: Int) extends HermitianOp
  object σ extends HermitianType1(1 to 3)
  val operators = Seq(σ)
}

import PauliFree.σ

def mod1(i: Int, n: Int): Int = ((i - 1) % n) + 1

val PauliQuotient = quotient.MonoidDef(PauliFree)(pairSubstitutions = {
  case (σ(i), σ(j)) if i == j => Mono.one
  case (σ(i), σ(j)) if mod1(i + 1, 3) == j => -Mono(σ(6 - i - j))*Phase.i
  case (σ(i), σ(j)) if mod1(i + 2, 3) == j => Mono(σ(6 - i - j))*Phase.i
})
```
Here is the result:
```tut
PauliQuotient.quotient(σ(1)*σ(2)*σ(3))
```
