

organization := "fr.iscpif"
name := "freedsl"

lazy val root = (project in file(".")).
  aggregate(
    random
  ) settings(
    publishArtifact := false
  )

def settings = Seq (
  scalaVersion := "2.11.8",
  scalaOrganization := "org.typelevel",
  organization := "fr.iscpif.freedsl",
  crossScalaVersions := Seq("2.11.8"),
  libraryDependencies += "com.projectseptember" %% "freek" % "0.6.0",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.2"),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  licenses := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/")),
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

lazy val random = Project(id = "random", base = file("random"), settings = settings)
lazy val log = Project(id = "log", base = file("log"), settings = settings)



