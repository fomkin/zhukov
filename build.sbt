import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.github.fomkin"
ThisBuild / organizationName := "zhukov"

lazy val protobuf = project
  .in(file("protobuf"))
  .settings(
    name := "zhukov-protobuf"
  )

lazy val core = project
  .in(file("core"))
  .settings(
    name := "zhukov-core"
  )
  .dependsOn(protobuf)

lazy val derivation = project
  .in(file("derivation"))
  .settings(Project.inConfig(Test)(sbtprotoc.ProtocPlugin.protobufConfigSettings):_*)
  .settings(
    name := "zhukov-derivation",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    PB.targets in Test := Seq(scalapb.gen() -> (sourceManaged in Test).value),
    PB.targets in Compile := Nil,
    PB.protoSources in Test := Seq(file("derivation/src/test/protobuf")),
    libraryDependencies := Seq(
      scalaPb % Test, utest, // testing
      macroCompat, macroParadise, scalaCompiler(scalaVersion.value) // macros
    )
  )
  .dependsOn(core)
lazy val root = (project in file("."))
  .aggregate(protobuf, core, derivation)
