val scala212Version = "2.12.6"

val alascVersion = "0.16.0.0"
val attributesVersion = "0.30"
val betterFilesVersion = "3.4.0"
val catsVersion = "1.1.0"
val disciplineVersion = "0.8"
val fastParseVersion = "1.0.0"
val jGraphTVersion = "1.2.0"
val jOptimizerVersion = "4.0.0"
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
  .aggregate(core, mosek)
  .dependsOn(core, mosek)

lazy val core = (project in file("core"))
  .settings(moduleName := "symdpoly-core")
  .settings(symdpolySettings)

// unmanagedJars in Compile += file(Path.userHome+"/software/mosek/8/tools/platform/linux64x86/bin/mosek.jar")

lazy val mosek = (project in file("mosek"))
  .settings(moduleName := "symdpoly-mosek")
  .settings(symdpolySettings)
  .dependsOn(core)

lazy val symdpolySettings = buildSettings ++ commonSettings ++ publishSettings

lazy val buildSettings = Seq(
  name := "symdpoly",
  organization := "net.alasc",
  scalaVersion := scala212Version
)

lazy val commonSettings = Seq(
//  apiURL := Some(url("https://denisrosset.github.io/metal/latest/api")),
  scmInfo := Some(ScmInfo(url("https://github.com/denisrosset/symdpoly"), "scm:git:git@github.com:denisrosset/symdpoly.git")),
//  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  scalacOptions ++= commonScalacOptions.diff(Seq(
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )) :+ "-opt:l:inline" :+ "-opt-inline-from:<sources>" :+ "-opt-warnings", // activate optimizations
  resolvers ++= Seq(
    "bintray/denisrosset/maven" at "https://dl.bintray.com/denisrosset/maven",
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  ),
  libraryDependencies ++= Seq(
    "com.diffplug.matsim" % "matfilerw" % matFileRWVersion,
    "com.github.pathikrit" %% "better-files" % betterFilesVersion,
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.typelevel" %% "cats-kernel" % catsVersion,
    "org.typelevel" %% "discipline" % disciplineVersion,
    "org.scala-metal" %% "metal-core" % metalVersion,
    "org.scala-metal" %% "metal-library" % metalVersion,
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion,
    "org.scalatest" %% "scalatest" % scalaTestVersion,
    "org.typelevel" %% "spire" % spireVersion,
    "net.alasc" %% "cyclo-core" % spireCycloVersion,
    "net.alasc" %% "cyclo-laws" % spireCycloVersion,
    "net.alasc" %% "alasc-core" % alascVersion,
    "net.alasc" %% "alasc-laws" % alascVersion,
    "net.alasc" %% "scalin-core" % scalinVersion,
    "net.alasc" %% "scalin-alasc" % scalinVersion,
    "com.chuusai" %% "shapeless" % shapelessVersion,
    "com.lihaoyi" %% "sourcecode" % sourcecodeVersion,
    "com.joptimizer" % "joptimizer" % jOptimizerVersion,
    "com.jsuereth" %% "scala-arm" % scalaARMVersion
  ),
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)

lazy val publishSettings = Seq(
  homepage := Some(url("http://denisrosset.github.io/symdpoly")),
  licenses += ("AGPL-V3", url("https://opensource.org/licenses/AGPL-3.0")),
  bintrayRepository := "maven",
  publishArtifact in Test := false
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
  "-Xlint",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-Xfuture"
)

lazy val noPublishSettings = Seq(
  publish := (()),
  publishLocal := (()),
  publishArtifact := false
)
