---
layout: home
title:  "Home"
section: "Home"
position: 0
---

![alt text](/symdpoly/img/symdpoly_logo.png "SymDPoly")

Current version: **{{site.symdpolyVersion}}** for **Scala {{site.scalaVersion}}**. 

[![Join the chat at https://gitter.im/denisrosset/symdpoly](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/denisrosset/symdpoly)
[![Travis CI](https://travis-ci.org/denisrosset/symdpoly.svg?branch=master)](https://travis-ci.org/denisrosset/symdpoly)

**SymDPoly** solves polynomial optimization problems using symmetry-adapted semidefinite relaxations. The problems can involve either commutative or noncommutative variables, though SymDPoly is optimized for the latter. SymDPoly does not require a particular structure in the problem, as it works by brute combinatorial enumeration of equivalences. However, its processing routines are highly optimized and able to reduce around 1M monomials/second.

## A simple example

An evaluation of the Tsirelson's bound for the CHSH inequality can be done in a few lines of code. See the [**full explanation**](docs/simple-example.html)

First, we define the free algebra for four operators `A(0)`, `A(1)`, `B(0)` and `B(1)`, corresponding to measurements with outcomes `+1` and `-1`. We import the operator types `A` and `B` in global scope.
```tut:silent
import net.alasc.symdpoly._; import net.alasc.symdpoly.joptimizer._

object Free extends free.MonoidDef {
  case class A(x: Int) extends HermitianOp
  object A extends HermitianType1(0 to 1)
  case class B(y: Int) extends HermitianOp
  object B extends HermitianType1(0 to 1)
  val operators = Seq(A, B)
}

import Free.{A, B}

val Quotient = quotient.MonoidDef(Free) {
  case (A(x1), A(x2)) if x1 == x2 => Mono.one
  case (B(y1), B(y2)) if y1 == y2 => Mono.one
  case (B(y), A(x))               => A(x) * B(y)
  case (op1, op2)                 => op1 * op2
}

val swapParties = Free.generator {
  case A(i) => B(i)
  case B(i) => A(i)
}

val ambientGroup = Free.ambientGroup(swapParties)

val chsh = Quotient.quotient(A(0) * B(0) + A(0) * B(1) + A(1) * B(0) - A(1) * B(1))

val generatingSet = Quotient.quotient(GSet.onePlus(A, B))

val L = evaluation.pureStateSelfAdjoint(Quotient)

val relaxation = L(chsh).maximize.symmetricRelaxation(generatingSet, ambientGroup)
```
We then solve:
```tut
relaxation.jOptimizerInstance.solve()
```

## How to start a SymDPoly project

See the [**Installation guide**](docs/installation.html).

## Work in progress

**SymDPoly** is a work-in-progress. While the underlying framework handles unrestricted polynomial optimization problems, its interface is currently limited to a subset of problems.

1. When defining the quotient algebra, only substitution rules involving pairs of variables can be defined.
2. It does not handle linear equality constraints.
3. It does not handle operator semidefinite constraints.
4. It does not handle scalar inequality constraints.
5. Only real semidefinite formulations can be outputted, per the formats of the solvers currently on the market.

In particular, the limitations 1., 4. and 5. are easy to add. The limitations 2. and 3. require specific handling with respect to the symmetries and are currently under active investigation.

## Why SymDPoly?

**SymDPoly** differs from existing semidefinite relaxation frameworks in a number of ways.

- It supports both noncommutative and commutative variables.
- It supports equality constraints through monomial rewriting, which leads to efficient formulations.
- It discovers symmetries of the problem and creates symmetry-adapted relaxations.
- It interfaces with a wide range of solvers.
- It works in exact arithmetic.
- It is extremely fast.

Other libraries working on the same problem space include:

- [ncpol2sdpa](https://gitlab.com/peterwittek/ncpol2sdpa), a package for Python (noncommutative/commutative).
- [NCSOSTools](http://ncsostools.fis.unm.si/), a toolbox for MATLAB (noncommutative).
- [GloptiPoly](http://homepages.laas.fr/henrion/software/gloptipoly/), a package for MATLAB (commutative).
- [SparsePOP](http://sparsepop.sourceforge.net/), a toolbox for MATLAB (commutative).
- [https://www.cds.caltech.edu/sostools/](SOSTOOLS), a toolbox for MATLAB (commutative).


## When using SymSDPoly as a library.

The current version is **{{site.symdpolyVersion}}** for **Scala {{site.scalaVersion}}**. 

It requires the following additions to the `build.sbt` file.

```scala
resolvers += Resolver.bintrayRepo("denisrosset", "maven")

libraryDependencies += "net.alasc" %% "symdpoly-core"    % "{{site.symdpolyVersion}}"
```

When integrating SymDPoly with other Scala libraries, note that it is based on the Spire library [spire](https://github.com/non/spire) version {{site.spireVersion}}, which in turn depends on [cats-kernel](https://typelevel.org/cats).

## Documentation and Support

- Chat it up on [Gitter](https://gitter.im/denisrosset/symdpoly).
- Check the [tutorial](https://denisrosset.github.io/symdpoly/docs/first-steps.html).
- Check the [Scaladoc](https://denisrosset.github.io/symdpoly/api).

## Contributors

SymDPoly and the group theory/linear algebra libraries it depends on were written by [Denis Rosset](https://github.com). For the implementation of the [cyclotomic number field](https://github.com/denisrosset/cyclo), we acknowledge contributions from the [GAP system](http://www.gap-system.org/Gap3/gap3.html) project.

Feedback and suggestions are always welcome. We ask participants to follow the guidelines of the [Typelevel Code of Conduct](https://typelevel.org/conduct.html) (note that **SymDPoly** is not a Typelevel project, but some of its components, e.g. **Spire** are).

## License

SymDPoly is (C) 2018 Denis Rosset and licensed under the [GNU Affero General Public License version 3 or any later version](https://github.com/denisrosset/symdpoly/LICENSE.md).
