
import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

scalaVersion in ThisBuild := "2.12.3"
crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.3")
scalacOptions in ThisBuild += "-Ypartial-unification"
organization in ThisBuild := "fr.iscpif.freedsl"
resolvers in ThisBuild += Resolver.bintrayRepo("projectseptemberinc", "maven")
libraryDependencies in ThisBuild += "com.projectseptember" %% "freek" % "0.6.7"
libraryDependencies in ThisBuild += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies in ThisBuild += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies in ThisBuild += "org.typelevel"  %% "squants"  % "1.3.0"
//  resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),
//  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-beta4" cross CrossVersion.full),

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
  ReleaseStep(action = Command.process("publishSigned", _)),
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
  pushChanges
)

def settings = Seq(
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
)

lazy val freedsl = Project(id = "dsl", base = file("dsl")) settings(settings: _*)

lazy val random = Project(id = "random", base = file("dsls/random")) settings(settings: _*) dependsOn(freedsl)
lazy val log = Project(id = "log", base = file("dsls/log")) settings(settings: _*) dependsOn(freedsl)
lazy val system = Project(id = "system", base = file("dsls/system")) settings(settings: _*) dependsOn(freedsl)
lazy val io = Project(id = "io", base = file("dsls/io")) settings(settings: _*) dependsOn(freedsl)
lazy val filesystem = Project(id = "filesystem", base = file("dsls/filesystem")) settings(settings: _*) dependsOn(freedsl)
lazy val errorhandler = Project(id = "errorhandler", base = file("dsls/errorhandler")) settings(settings: _*) dependsOn(freedsl)
lazy val tool = Project(id = "tool", base = file("tool")) settings(settings: _*)

lazy val example = Project(id = "example", base = file("example")) settings(settings: _*) dependsOn(random, system, log)



