
import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

scalaVersion in ThisBuild := "2.12.3"
crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.3")
scalacOptions in ThisBuild += "-Ypartial-unification"
organization in ThisBuild := "fr.iscpif.freedsl"

libraryDependencies in ThisBuild += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}
publishMavenStyle in ThisBuild := true
publishArtifact in Test in ThisBuild := false
pomIncludeRepository in ThisBuild := { _ => false }
licenses in ThisBuild := Seq("LGPL" -> url("http://www.gnu.org/licenses/"))
homepage in ThisBuild := Some(url("https://github.com/ISCPIF/freedsl"))
scmInfo in ThisBuild := Some(ScmInfo(url("https://github.com/ISCPIF/freedsl.git"), "scm:git:git@github.com:ISCPIF/freedsl.git"))
pomExtra in ThisBuild := (
  <developers>
    <developer>
      <id>romainreuillon</id>
      <name>Romain Reuillon</name>
    </developer>
  </developers>
)


releaseVersionBump := sbtrelease.Version.Bump.Minor

releaseTagComment    := s"Releasing ${(version in ThisBuild).value}"

releaseCommitMessage := s"Bump version to ${(version in ThisBuild).value}"

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  tagRelease,
  releaseStepCommand("publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)

def settings = scalariformSettings(autoformat = true) ++ Seq (
  // macro paradise doesn't work with scaladoc
  sources in (Compile, doc) := Nil,
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
 // addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  libraryDependencies += "io.frees" %% "frees-tagless" % "0.4.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

lazy val freedsl = Project(id = "dsl", base = file("dsl")) settings(settings: _*)

lazy val random = Project(id = "random", base = file("random")) settings(settings: _*) dependsOn(freedsl)
lazy val log = Project(id = "log", base = file("log")) settings(settings: _*) dependsOn(freedsl)
lazy val system = Project(id = "system", base = file("system")) settings(settings: _*) settings (
  libraryDependencies += "org.typelevel"  %% "squants"  % "1.3.0"
) dependsOn(freedsl)

lazy val io = Project(id = "io", base = file("io")) settings(settings: _*) dependsOn(freedsl)
lazy val filesystem = Project(id = "filesystem", base = file("filesystem")) settings(settings: _*)dependsOn(freedsl)
lazy val errorhandler = Project(id = "errorhandler", base = file("errorhandler")) settings(settings: _*)  dependsOn(freedsl)
lazy val tool = Project(id = "tool", base = file("tool")) settings(settings: _*) dependsOn(random % "test")
lazy val example = Project(id = "example", base = file("example")) settings(settings: _*) dependsOn(random, system, log)



