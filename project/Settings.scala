import sbt._
import Keys._
import sbtassembly.AssemblyPlugin.autoImport._

object Settings {

  lazy val settings = Seq(
    organization := "com.root_corporation",
    version := "0.0.1" + sys.props
      .getOrElse("buildNumber", default = "0-SNAPSHOT"),
    scalaVersion := "2.13.2",
    publishMavenStyle := true,
    publishArtifact in Test := false
  )

  lazy val testSettings = Seq(
    fork in Test := false,
    parallelExecution in Test := false
  )

  lazy val itSettings = Defaults.itSettings ++ Seq(
    logBuffered in IntegrationTest := false,
    fork in IntegrationTest := true
  )

  lazy val goalsSettings = Seq(
    assemblyJarName in assembly := "goals-" + version.value + ".jar",
    test in assembly := {},
    target in assembly := file(baseDirectory.value + "/../bin/"),
    assemblyOption in assembly := (assemblyOption in assembly).value
      .copy(includeScala = false, includeDependency = true),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*)       => MergeStrategy.discard
      case n if n.startsWith("reference.conf") => MergeStrategy.concat
      case _                                   => MergeStrategy.first
    }
  )

}
