---
layout: docs
title: Interfacing solvers
position: 2
---

# {{page.title}}

As Scala is a relatively niche programming language in the physics community, we expect our users to use **SymDPoly** as a domain-specific tool (where Scala shines as a domain-specific language with notation close to the mathematical notation), and immediately export the data after the processing is done. Thus **SymDPoly** can output the problem data in a variety of formats.

In the following, we assume that the variable `program` contains the conic linear program corresponding to the current moment-based relaxation.

```tut:silent
import net.alasc.symdpoly._
import net.alasc.symdpoly.examples.quantum.CHSH
import CHSH._
val L = Quantum.evaluator(evaluation.real)
val generatingSet = Quantum.quotient(GSet.onePlus(Free.A, Free.B))
val program = L(Quantum.quotient(CHSH.chsh)).maximize.relaxation(generatingSet).program
```

## Solver input data: text-based formats

- Mosek input file in the [Conic Benchmark Format](https://docs.mosek.com/8.1/toolbox/cbf-format.html#doc-shared-cbfformat), which is a text based format. Use:

```tut:silent
program.mosek.writeFile("chsh_mosek.cbf")
```

- Sparse [SDPA](http://sdpa.sourceforge.net/) input file (text based format again). Use:

```tut:silent
program.sdpa.writeFile("chsh_sdpa.dat-s")
```

## Solver input data: MAT file-based formats

Those solvers need to be called from Matlab. For those, **SymDPoly** saves `.mat` files containing the argument values needed to call the main solver function.

- [SCS](https://github.com/cvxgrp/scs), using the [SCS-matlab](https://github.com/bodono/scs-matlab) interface. 


```tut:silent
program.scs.writeFile("chsh_scs.mat")
```

- SDPT3 format file, which can be used either with [SDPT3](http://www.math.nus.edu.sg/~mattohkc/sdpt3.html), [SDPNAL](http://www.math.nus.edu.sg/~mattohkc/SDPNAL.html) (untested) or [SDPNAL+](http://www.math.nus.edu.sg/~mattohkc/SDPNALplus.html). 

```tut:silent
program.sdpt3.writeFile("chsh_sdpt3.mat")
```

- [SeDuMi](http://sedumi.ie.lehigh.edu/) format file; note that the SeDuMi file format contains information about the symmetries of the semidefinite constraints.

```tut:silent
program.sedumi.writeFile("chsh_sedumi.mat")
```

## Java native solver

**SymDPoly** can interface with the [JOptimizer](http://www.joptimizer.com/) package, able to solve semidefinite programs using a Barrier Method. Use with toy problems only.

## Running Mosek directly (native problem formats)

You want to run the Mosek solver directly, without writing intermediate files, or write problems in the [Task Format](https://docs.mosek.com/8.1/toolbox/task-format.html#doc-shared-taskformat) or [JSON Format](https://docs.mosek.com/8.1/toolbox/json-format.html#doc-shared-jtaskformat) formats.

For that, you need to interface Mosek with the Java Virtual Machine. First, import the `symdpoly-mosek` module in your `build.sbt` file.
```scala
libraryDependencies += "net.alasc" %% "symdpoly-mosek"    % "{{site.symdpolyVersion}}"
```

Then, add a `lib` subfolder at the root of your project, which contains the JAR files and shared libraries of `mosek/8/tools/platform/XXX/bin` from your Mosek installation.

If you are running Linux, you may want to add that `lib` subfolder to `LD_LIBRARY_PATH` as well.

Then, you can call:

```tut
import net.alasc.symdpoly.mosek._
program.nativeMosek.solve()
```
to run Mosek directly, and
```tut:silent
program.nativeMosek.writeFile("chsh_mosek.task")
program.nativeMosek.writeFile("chsh_mosek.jtask")
```
to write files in any of the Mosek supported formats.
