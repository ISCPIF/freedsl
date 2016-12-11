
def modules = Seq(dsl, random, log, util)

lazy val root =
  Project(id = "all", base = file(".")).settings(settings: _*).
    aggregate(modules.map(_.project): _*).dependsOn(modules.map(p => p: ClasspathDep[ProjectReference]): _*)

def settings = Seq (
  scalaVersion := "2.12.0",
  crossScalaVersions := Seq("2.11.8", "2.12.0"),
  scalaOrganization := "org.typelevel",
  scalacOptions += "-Ypartial-unification",
  organization := "fr.iscpif.freedsl",
  resolvers += Resolver.bintrayRepo("projectseptemberinc", "maven"),
  libraryDependencies += "com.projectseptember" %% "freek" % "0.6.5",
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-core" % "1.3.2",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
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
lazy val random = Project(id = "random", base = file("random")).settings(settings: _*) dependsOn(dsl)
lazy val log = Project(id = "log", base = file("log")).settings(settings: _*) dependsOn(dsl)
lazy val util = Project(id = "util", base = file("util")).settings(settings: _*) dependsOn(dsl)

lazy val example = Project(id = "example", base = file("example")).settings(settings: _*) dependsOn(random, util, log)



