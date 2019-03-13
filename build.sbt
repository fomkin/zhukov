import Dependencies._
import xerial.sbt.Sonatype._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / organization     := "com.github.fomkin"

val unusedRepo = Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

val publishSettings = Seq(
  publishTo := sonatypePublishTo.value,
  publishArtifact in Test := false,
  publishMavenStyle := true,
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  //headerLicense := Some(HeaderLicense.ALv2("2017-2019", "Aleksey Fomkin")),
  //excludeFilter.in(headerSources) := HiddenFileFilter || "IntStringMap.scala",
  sonatypeProjectHosting := Some(GitHubHosting("fomkin", "zhukov", "Aleksey Fomkin", "aleksey.fomkin@gmail.com"))
)

val dontPublishSettings = Seq(
  publish := {},
  publishTo := unusedRepo,
  publishArtifact := false
//  headerLicense := None
)

lazy val protobuf = project
  .in(file("protobuf"))
  .enablePlugins(GitVersioning)
  .settings(publishSettings: _*)
  .settings(
    name := "zhukov-protobuf"
  )

lazy val core = project
  .in(file("core"))
  .enablePlugins(GitVersioning)
  .settings(publishSettings: _*)
  .settings(
    name := "zhukov-core"
  )
  .dependsOn(protobuf)

lazy val derivation = project
  .in(file("derivation"))
  .enablePlugins(GitVersioning)
  .settings(publishSettings: _*)
  .settings(Project.inConfig(Test)(sbtprotoc.ProtocPlugin.protobufConfigSettings):_*)
  .settings(
    name := "zhukov-derivation",
    testFrameworks += new TestFramework("minitest.runner.Framework"),
    PB.targets in Test := Seq(scalapb.gen() -> (sourceManaged in Test).value),
    PB.targets in Compile := Nil,
    PB.protoSources in Test := Seq(file("derivation/src/test/protobuf")),
    libraryDependencies := Seq(
      scalaPb % Test, minitest, // testing
      macroCompat, macroParadise, scalaCompiler(scalaVersion.value) // macros
    )
  )
  .dependsOn(core)

lazy val root = (project in file("."))
  .settings(dontPublishSettings:_*  )
  .aggregate(protobuf, core, derivation)
