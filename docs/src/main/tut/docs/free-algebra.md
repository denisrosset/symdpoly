---
layout: docs
title: "Details: free algebra"
---

# {{page.title}}

A [free algebra](https://en.wikipedia.org/wiki/Free_algebra) is a polynomial ring in noncommutative variables. Variables are standard Scala objects and can bear indices and flags. Those objects must exist in an object inheriting `free.MonoidDef`, and themselves inherit `Op`.

Free algebras are defined as [monoid rings](https://en.wikipedia.org/wiki/Monoid_ring) over a free monoid (with little added mathematical structure, see below); the ring we use is the field of [cyclotomic numbers](https://en.wikipedia.org/wiki/Cyclotomic_field).

## Definitions

We return to our CHSH example, but removing the syntactic sugar present in the [simple example](simple-problem.html):

```tut:silent
import net.alasc.symdpoly._
import math.Phase
import defaults._

object Free extends free.MonoidDef(cyclotomicOrder = 2) {
  case class A(x: Int) extends Op {
	def adjoint: Op = this
  }
  object A extends OpFamily {
	val allInstances = Seq(A(0), A(1))
  }
  case class B(y: Int) extends Op {
    def adjoint: Op = this
  }
  object B extends OpFamily {
    val allInstances = Seq(B(0), B(1))
  }
  val operators = Seq(A, B)
}
import Free.{A, B}
```

```tut
(A(0)*B(0)).adjoint
```
We can also represent non-Hermitian variables:

```tut:silent
object Free1 extends free.MonoidDef(cyclotomicOrder = 4) {
  case class X(i: Int, isAdjoint: Boolean = false) extends Op {
    override def toString = if (isAdjoint) s"X($i)*" else s"X($i)"
    def adjoint: Op = X(i, !isAdjoint)
  }
  object X extends OpFamily {
    val allInstances = Seq(X(0), X(1), X(2), X(0).adjoint, X(1).adjoint, X(2).adjoint)
  }
  val operators = Seq(X)
}
import Free1.X
```

```tut
(X(0)*X(1)).adjoint
```

Note that Scala's type system prevents us from mixing operators coming from different free monoids:
```tut:fail
X(0)*A(0)
```


## Syntactic sugar

To ease the boilerplate of defining `allInstances` and the `adjoint` method for Hermitian operators, we can use the following shortcuts.

```tut:silent
object FreeSimplified extends free.MonoidDef(2) {
  case class A(x: Int) extends HermitianOp // defines an `adjoint` method that is the identity
  object A extends HermitianOpFamily1(0 to 1)  // enumerates the instances from a given Range, if the operator has a single index
  case class B(y: Int) extends HermitianOp
  object B extends HermitianOpFamily1(0 to 1)
  val operators = Seq(A, B)
}
```

The same holds for non-Hermitian variables.

```tut:silent
object Free1Simplified extends free.MonoidDef(4) {
  case class X(i: Int, isAdjoint: Boolean = false) extends Op {
    override def toString = if (isAdjoint) s"X($i)*" else s"X($i)"
    def adjoint: Op = X(i, !isAdjoint)
  }
  object X extends OpFamily1(0 to 2)
  val operators = Seq(X)
}
```

```tut
Free1Simplified.X.allInstances
```

## Additional structure compared to free monoids

Compared to standard `*`-free monoids, we already include the following structure *at the level of our free monoids*.

### Our free monoids are `*`-monoids

Our monoids have an involution compatible with multiplication.

```tut
val x = X(0)*X(1).adjoint
val y = X(1)*X(0)

(x*y).adjoint
(y.adjoint)*(x.adjoint)
```

### Self-adjoint elements are handled

Instead of defining an equivalence rule `X(0) ~ X(0).adjoint` at the level of the quotient monoid, we already have an identity between operator variables and their adjoints when the variable is Hermitian.

```tut
A(0).adjoint
```

### Our free monoids are binoids

Binoids are monoids with an absorbing element, which we denote by *zero* (see the thesis of [Simone Boettger](https://arxiv.org/pdf/1603.02093.pdf)). This absorbing element is unique.

```tut
Free1.zero * X(0)
X(0) * Free1.zero
Free1.zero * Free1.zero
```

## Polynomials over free monoids

First and foremost, we can have polynomials whose monomials are elements of a free monoid (excluding zero):

```tut
val p = A(0) * B(0) + A(1) * B(0) + A(0) * B(1) - A(1) * B(1)
```

```tut
p * p - p
```

Due to syntax issues in the Scala programming language, you must multiply by scalars *on the right*, or either use the left multiplication `*:` operator.

```tut
p * sqrt(2)
sqrt(2) *: p
```

Due to the same syntax issue, constant terms must be added from the right:

```tut
p - sqrt(2)*2
```

or use a cumbersome syntax:
```tut
Free.one*(sqrt(2)*2)
Free.one*(sqrt(2)*2) - p
```

This will fail:
```tut:fail
sqrt(2)*2 - p
```
as for binary operations, the left term handles the evaluation, and thus it should be the most complex type present.

## Other objects on free monoids

Likewise 
Other objects include operators with a phase, used to define symmetries:
```tut
A(0) -> -A(0)
```
and monomials with a phase, useful to define substitution rules:
```tut
X(0) * X(1) -> X(2) * Phase.minusI
```

When constructing the moment matrix, **SymDPoly* processes monomials with a phase.
