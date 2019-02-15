import microsites._
import ReleaseTransformations._

val scala212Version = "2.12.8"

val alascVersion = "0.16.0.4-SNAPSHOT"
val attributesVersion = "0.30"
val betterFilesVersion = "3.4.0"
val catsVersion = "1.1.0"
val disciplineVersion = "0.8"
val fastParseVersion = "1.0.0"
val jGraphTVersion = "1.2.0"
val jOptimizerVersion = "4.0.0"
val kindProjectorVersion = "0.9.8"
val matFileRWVersion = "3.0.1"
val metalVersion = "0.16.0.0"
val progressBarVersion = "0.7.0"
val scalaARMVersion = "2.0"
val scalaCheckVersion = "1.13.5"
val scalaTestVersion = "3.0.5"
val scalinVersion = "0.16.0.0"
val shapelessVersion = "2.3.3"
val sourcecodeVersion = "0.1.4"
val spireVersion = "0.16.0"
val spireCycloVersion = "0.16.0.0"

lazy val symdpoly = (project in file("."))
  .settings(moduleName := "symdpoly")
  .settings(symdpolySettings)
  .settings(noPublishSettings)
  .aggregate(core, mosek, jOptimizer, matlab, examples, tests)
  .dependsOn(core, mosek, jOptimizer, matlab, examples, tests)

lazy val docs = (project in file("docs"))
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(moduleName := "symdpoly-docs")
  .settings(symdpolySettings)
  .settings(noPublishSettings)
  .settings(docSettings)
  .dependsOn(core, mosek, jOptimizer, matlab, examples)

lazy val core = (project in file("core"))
  .settings(moduleName := "symdpoly-core")
  .settings(symdpolySettings)

lazy val matlab = (project in file("matlab"))
  .settings(moduleName := "symdpoly-matlab")
  .settings(symdpolySettings)
  .settings(matlabSettings)
  .dependsOn(core)

lazy val mosek = (project in file("mosek"))
  .settings(moduleName := "symdpoly-mosek")
  .settings(symdpolySettings)
  .dependsOn(core)

lazy val jOptimizer = (project in file("joptimizer"))
  .settings(moduleName := "symdpoly-joptimizer")
  .settings(symdpolySettings)
  .settings(jOptimizerSettings)
  .dependsOn(core)

lazy val tests = (project in file("tests"))
  .settings(moduleName := "symdpoly-tests")
  .settings(symdpolySettings)
  .settings(testsSettings)
  .dependsOn(core, jOptimizer, matlab)

lazy val examples = (project in file("examples"))
  .settings(moduleName := "symdpoly-examples")
  .settings(noPublishSettings)
  .settings(symdpolySettings)
  .settings(testsSettings)
  .dependsOn(core, jOptimizer, matlab)

lazy val symdpolySettings = buildSettings ++ commonSettings ++ publishSettings

lazy val buildSettings = Seq(
  name := "symdpoly",
  organization := "net.alasc",
  scalaVersion := scala212Version
)

lazy val matlabSettings = Seq(
  libraryDependencies ++= Seq(
    "com.diffplug.matsim" % "matfilerw" % matFileRWVersion,
  )
)

lazy val jOptimizerSettings = Seq(
  libraryDependencies ++= Seq(
    "com.joptimizer" % "joptimizer" % jOptimizerVersion
  )
)

lazy val testsSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion,
    "org.scalatest" %% "scalatest" % scalaTestVersion,
    "org.typelevel" %% "discipline" % disciplineVersion,
    "net.alasc" %% "cyclo-laws" % spireCycloVersion,
    "net.alasc" %% "alasc-laws" % alascVersion
  )
)

lazy val commonSettings = Seq(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion),
  apiURL := Some(url("https://denisrosset.github.io/symdpoly/api")),
  scmInfo := Some(ScmInfo(url("https://github.com/denisrosset/symdpoly"), "scm:git:git@github.com:denisrosset/symdpoly.git")),
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  scalacOptions ++= commonScalacOptions.diff(Seq(
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )),
  resolvers ++= Seq(
    "bintray/denisrosset/maven" at "https://dl.bintray.com/denisrosset/maven",
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  ),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.scala-metal" %% "metal-core" % metalVersion,
    "org.scala-metal" %% "metal-library" % metalVersion,
    "org.typelevel" %% "spire" % spireVersion,
    "net.alasc" %% "cyclo-core" % spireCycloVersion,
    "net.alasc" %% "alasc-core" % alascVersion,
    "net.alasc" %% "scalin-core" % scalinVersion,
    "com.chuusai" %% "shapeless" % shapelessVersion,
    "com.lihaoyi" %% "sourcecode" % sourcecodeVersion,
    "com.jsuereth" %% "scala-arm" % scalaARMVersion
  ),
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)

lazy val publishSettings = Seq(
  homepage := Some(url("http://denisrosset.github.io/symdpoly")),
  licenses += ("AGPL-V3", url("https://opensource.org/licenses/AGPL-3.0")),
  bintrayRepository := "maven",
  publishArtifact in Test := false,
  bintrayReleaseOnPublish in ThisBuild := false,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    releaseStepCommand("docs/tut"), // annoying that we have to do this twice
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    releaseStepCommand("bintrayRelease"),
    releaseStepCommand("docs/publishMicrosite"),
    setNextVersion,
    commitNextVersion,
    pushChanges
  )
)


lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")

/**
  * Remove 2.10 projects from doc generation, as the macros used in the projects
  * cause problems generating the documentation on scala 2.10. As the APIs for 2.10
  * and 2.11 are the same this has no effect on the resultant documentation, though
  * it does mean that the scaladocs cannot be generated when the build is in 2.10 mode.
  */
def docsSourcesAndProjects(sv: String): (Boolean, Seq[ProjectReference]) =
  CrossVersion.partialVersion(sv) match {
    case Some((2, 10)) => (false, Nil)
    case _ => (true, Seq(core, matlab, jOptimizer, mosek))
  }

lazy val docSettings = Seq(
  micrositeName := "SymDPoly",
  micrositeDescription := "Symmetry-adapted moment relaxations for noncommutative polynomial optimization",
  micrositeAuthor := "Denis Rosset",
  micrositeHighlightTheme := "atom-one-light",
  micrositeHomepage := "http://denisrosset.github.io/symdpoly",
  micrositeBaseUrl := "symdpoly",
  micrositeDocumentationUrl := "/symdpoly/api/net/alasc/symdpoly/index.html",
  micrositeDocumentationLabelDescription := "API Documentation",
  micrositeGithubOwner := "denisrosset",
  micrositeGithubRepo := "symdpoly",
  micrositePalette := Map(
    "brand-primary" -> "#5B5988",
    "brand-secondary" -> "#292E53",
    "brand-tertiary" -> "#222749",
    "gray-dark" -> "#49494B",
    "gray" -> "#7B7B7E",
    "gray-light" -> "#E5E5E6",
    "gray-lighter" -> "#F4F3F4",
    "white-color" -> "#FFFFFF"),
  micrositeConfigYaml := ConfigYml(
    yamlCustomProperties = Map(
      "symdpolyVersion"    -> version.value,
      "spireVersion"    -> spireVersion,
      "scalaVersion"  -> scalaVersion.value
    )
  ),
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) :=
    inProjects(docsSourcesAndProjects(scalaVersion.value)._2:_*),
  docsMappingsAPIDir := "api",
  addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), docsMappingsAPIDir),
  ghpagesNoJekyll := false,
  fork := true,
  javaOptions += "-Xmx4G", // to have enough memory in forks
//  fork in tut := true,
//  fork in (ScalaUnidoc, unidoc) := true,
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-Xfatal-warnings",
    "-groups",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams"
  ),
  scalacOptions in Tut ~= (_.filterNot(Set("-Ywarn-unused-import", "-Ywarn-dead-code"))),
  git.remoteRepo := "git@github.com:denisrosset/symdpoly.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md" | "*.svg",
  includeFilter in Jekyll := (includeFilter in makeSite).value
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

lazy val noPublishSettings = Seq(
  publish := (()),
  bintrayRelease := (()),
  publishLocal := (()),
  publishArtifact := false
)
