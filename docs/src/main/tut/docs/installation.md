---
layout: docs
title: Installation
position: 1
---

# {{page.title}}

In this chapter we cover the basics of starting a SymDPoly project.

There are two options:

1. Run simple command line scripts using Ammonite (best for one-off computations)
2. Work with a full SBT project (best for multi file projects, and for use with an IDE)

## Simple command line scripts (Ammonite)

First, install the [Ammonite REPL](https://ammonite.io/#Ammonite-REPL).

- On Linux/Mac OS X, follow the instructions [here](https://ammonite.io/#Ammonite-REPL).
- On Windows, download the [latest release](https://github.com/lihaoyi/Ammonite/releases) -- the filename should look like `2.12-1.6.5`, where the `1.6.5` can change depending on the Ammonite version. Rename the file to `amm.bat`, and place it either in your path, or in the directory where you want to use it.

You'll also need a recent version of the Java Virtual Machine (minimum Java 1.8).
To verify this, you can run `java -version` from the command line, and you should obtain an output similar to
```
openjdk version "1.8.0_191"
OpenJDK Runtime Environment (build 1.8.0_191-8u191-b12-2ubuntu0.18.04.1-b12)
OpenJDK 64-Bit Server VM (build 25.191-b12, mixed mode)
```

Then, download one of the example scripts, for example [examples/CHSH.sc](https://github.com/denisrosset/symdpoly/blob/master/examples/CHSH.sc), and run it from the command line using `amm CHSH.sc`.

You should obtain the following output:

```
Compiling /home/denis/w/symdpoly/examples/CHSH.sc
Compiling /home/denis/w/symdpoly/examples/CHSH.sc #2
We maximize the CHSH expression [A(0) B(0)] + [A(0) B(1)] + [A(1) B(0)] - [A(1) B(1)]
We discovered a symmetry group of order 16 with generators Vector({A(0) -> - B(1), A(1) -> B(0), B(0) -> A(1), B(1) -> - A(0)}, {A(0) -> - A(1), A(1) -> - A(0), B(0) -> - B(0)})
Resulting relaxation: Moment relaxation with 1 monomials, a moment matrix of size 5 x 5, and 0 localizing matrix/ces
We wrote SDP data description files for SDPA (chsh.dat-s), Mosek (chsh.cbf) and SeDuMi (chsh.sedumi.mat)
```

NB: older versions of Ammonite (until version 1.6.4) used to send [usage statistics to Google Analytics by default](https://github.com/lihaoyi/Ammonite/issues/607), with a possibility to opt-out (`--no-remote-logging`).

## Using SymDPoly as part of a Scala project (SBT)

First, install the SBT build tool. While the download page is [here](https://www.scala-sbt.org/download.html), specific instructions are available:

- [For Linux.](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Linux.html)

- [For macOS.](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Mac.html)

- [For Windows.](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Windows.html)

### Creating a project

Start with an empty folder, and add the following `build.sbt` file in it.

```scala
resolvers += Resolver.bintrayRepo("denisrosset", "maven")

libraryDependencies += "net.alasc" %% "symdpoly-core"    % "{{site.symdpolyVersion}}"
```

Add now a single file `Test.scala` to your project:
```scala
import net.alasc.symdpoly._
import net.alasc.symdpoly.defaults._

object Test extends App {
  print(examples.quantum.CHSH.relaxation.program.sdpa.data)
}
```

And enter `sbt run` in the command line. After a few lines concerning package retrieval and compilation, you should see:
```
[info] Running Test 
* SDPA solves a minimization dual problem, while we express a maximization problem 
* also, the original objective has constant term cte = 0.0 (constant terms are not supported by SDPA)
* the real objective is thus cte - obj_SDPA value
1
1
5
4.0
0 1 1 1 -1.0
0 1 2 2 -1.0
0 1 3 3 -1.0
0 1 4 4 -1.0
0 1 5 5 -1.0
1 1 2 4 -1.0
1 1 3 4 -1.0
1 1 2 5 -1.0
1 1 3 5 1.0
```

This output corresponds to the problem data in SDPA sparse format, as requested.
