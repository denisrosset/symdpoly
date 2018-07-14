---
layout: docs
title: Installation
position: 1
---

# {{page.title}}

In this chapter we cover the basics of starting a SymDPoly project.

First, install the SBT build tool. While the download page is [here](https://www.scala-sbt.org/download.html), specific instructions are available:

- [For Linux.](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Linux.html)

- [For macOS.](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Mac.html)

- [For Windows.](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Windows.html)

## Creating a project

Start with an empty folder, and add the following `build.sbt` file in it.

```scala
resolvers += Resolver.bintrayRepo("denisrosset", "maven")

libraryDependencies ++= Seq(
  "net.alasc" %% "symdpoly-core"    % "{{site.symdpolyVersion}}",
  "net.alasc" %% "symdpoly-joptimizer"    % "{{site.symdpolyVersion}}",
  "net.alasc" %% "symdpoly-matlab"    % "{{site.symdpolyVersion}}"
)
```

Add now a single file `Test.scala` to your project:
```scala
import net.alasc.symdpoly._
import net.alasc.symdpoly.defaults._

object Test extends App {
  print(examples.CHSH.relaxation.sdpaInstance.data)
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

## Working with the Scala Read–Eval–Print Loop (REPL)

Now that we are confident that the toolbox works on your computer as expected, we will perform our future exploration using the REPL. At any time, you can cut'n'paste all the statements that have being used inside a `object MyApp extends App { ... }` in a Scala source code file and run it with `sbt run`. If you have multiple `App` objects, SBT will ask you which one you want to run.

Thus the example above would be presented:

```tut
import net.alasc.symdpoly._;
import net.alasc.symdpoly.defaults._;

examples.CHSH.relaxation.sdpaInstance.data
```
