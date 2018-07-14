---
layout: docs
title: Interfacing solvers
---

# {{page.title}}

As Scala is a relatively niche programming language in the physics community, we expect our users to use **SymDPoly** as a domain-specific tool (where Scala shines as a domain-specific language with notation close to the mathematical notation), and immediately export the data after the processing is done. Thus **SymDPoly** can output the problem data in a variety of formats.

In the following, we assume that the variable `relaxation` contains the SDP relaxation.

```tut:silent
val relaxation = net.alasc.symdpoly.examples.CHSH.relaxation
```

## Solver input data: text-based formats

- Mosek input file in the [Conic Benchmark Format](https://docs.mosek.com/8.1/toolbox/cbf-format.html#doc-shared-cbfformat). Use:

```tut:silent
relaxation.mosekInstance.writeCBF("chsh_mosek.cbf")
```

- Sparse [SDPA](http://sdpa.sourceforge.net/) input file. Use:

```tut:silent
 relaxation.sdpaInstance.writeFile("chsh_sdpa.dat-s")
```

## Solver input data: MAT file-based formats

These require the `symdpoly-matlab` module, and the following import:

```tut:silent
import net.alasc.symdpoly.matlab._
```

Then we can export the problem in the formats below.

- [SCS](https://github.com/cvxgrp/scs), using the [SCS-matlab](https://github.com/bodono/scs-matlab) interface. 


```tut:silent
relaxation.scsInstance.writeFile("chsh_scs.mat")
```

- SDPT3 format file, which can be used either with [SDPT3](http://www.math.nus.edu.sg/~mattohkc/sdpt3.html), [SDPNAL](http://www.math.nus.edu.sg/~mattohkc/SDPNAL.html) (untested) or [SDPNAL+](http://www.math.nus.edu.sg/~mattohkc/SDPNALplus.html). 

```tut:silent
relaxation.sdpt3Instance.writeFile("chsh_sdpt3.mat")
```

- [SeDuMi](http://sedumi.ie.lehigh.edu/) format file. 

```tut:silent
relaxation.sedumiInstance.writeFile("chsh_sedumi.mat")
```

## Java native solver

For test purposes, **SymDPoly** can interface with the [JOptimizer](http://www.joptimizer.com/) package, able to solve semidefinite programs using a Barrier Method. For that, add the `symdpoly-joptimizer` package to your `build.sbt` and run:

```tut:silent
import net.alasc.symdpoly.joptimizer._
relaxation.jOptimizerInstance.solve()
```

## Running Mosek directly (native problem formats)

You want to run the Mosek solver directly, without writing intermediate files, or write problems in the [Task Format](https://docs.mosek.com/8.1/toolbox/task-format.html#doc-shared-taskformat) or [JSON Format](https://docs.mosek.com/8.1/toolbox/json-format.html#doc-shared-jtaskformat) formats.

For that, you need to interface Mosek with the Java Virtual Machine. First, import the `symdpoly-mosek` module in your `build.sbt` file.
```scala
libraryDependencies += "net.alasc" %% "symdpoly-mosek"    % "{{site.symdpolyVersion}}"
```

Then, add a `lib` subfolder at the root of your project, which contains the JAR files and shared libraries of `mosek/8/tools/platform/XXX/bin` from your Mosek installation.

Then, you can call:

```tut
import net.alasc.symdpoly.mosek._
relaxation.mosekInstance.solve()
```
to run Mosek directly, and
```tut:silent
relaxation.mosekInstance.writeFile("chsh_mosek.task")
relaxation.mosekInstance.writeFile("chsh_mosek.jtask")
```
to write files in any of the Mosek supported formats.
