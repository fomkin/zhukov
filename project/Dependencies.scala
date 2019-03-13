import sbt._

object Dependencies {
  
  lazy val scalaPb = "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion
  lazy val minitest = "io.monix" %% "minitest" % "2.3.2" % Test
  lazy val macroCompat = "org.typelevel" %% "macro-compat" % "1.1.1"
  lazy val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)

  def scalaCompiler(scalaVersion: String): ModuleID = "org.scala-lang" % "scala-compiler" % scalaVersion % "provided"
}
