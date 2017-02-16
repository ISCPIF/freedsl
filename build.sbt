
def modules = Seq(dsl, random, log, system, io, filesystem, tool, errorhandler)

lazy val root =
  Project(id = "all", base = file(".")).settings(settings: _*).
    aggregate(modules.map(_.project): _*).dependsOn(modules.map(p => p: ClasspathDep[ProjectReference]): _*)

def settings = Seq (
  scalaVersion := "2.12.1",
  crossScalaVersions := Seq("2.11.8", "2.12.0"),
  scalaOrganization := "org.typelevel",
  scalacOptions += "-Ypartial-unification",
  organization := "fr.iscpif.freedsl",
  resolvers += Resolver.bintrayRepo("projectseptemberinc", "maven"),
  libraryDependencies += "com.projectseptember" %% "freek" % "0.6.6",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "org.typelevel"  %% "squants"  % "1.0.0",
//  resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),
//  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-beta4" cross CrossVersion.full),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  licenses := Seq("LGPL" -> url("http://www.gnu.org/licenses/")),
  homepage := Some(url("https://github.com/ISCPIF/freedsl")),
  scmInfo := Some(ScmInfo(url("https://github.com/ISCPIF/freedsl.git"), "scm:git:git@github.com:ISCPIF/freedsl.git")),
  pomExtra := (
    <developers>
      <developer>
        <id>romainreuillon</id>
        <name>Romain Reuillon</name>
      </developer>
    </developers>
  )
)


lazy val dsl = Project(id = "dsl", base = file("dsl")).settings(settings: _*)

lazy val random = Project(id = "random", base = file("dsls/random")).settings(settings: _*) dependsOn(dsl)
lazy val log = Project(id = "log", base = file("dsls/log")).settings(settings: _*) dependsOn(dsl)
lazy val system = Project(id = "system", base = file("dsls/system")).settings(settings: _*) dependsOn(dsl)
lazy val io = Project(id = "io", base = file("dsls/io")).settings(settings: _*) dependsOn(dsl)
lazy val filesystem = Project(id = "filesystem", base = file("dsls/filesystem")).settings(settings: _*) dependsOn(dsl)
lazy val errorhandler = Project(id = "errorhandler", base = file("dsls/errorhandler")).settings(settings: _*) dependsOn(dsl)

lazy val tool = Project(id = "tool", base = file("tool")).settings(settings: _*)

lazy val example = Project(id = "example", base = file("example")).settings(settings: _*) dependsOn(random, system, log)



