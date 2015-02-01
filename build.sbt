name := "Ariadne"

organization := "com.faacets"

version := "0.1.0"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")

licenses := Seq("MIT" -> url("http://opensource.org/licenses/mit-license.php"))

homepage := Some(url("https://github.com/denisrosset/ariadne"))

initialCommands in console := """import com.faacets.ariadne._"""

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.9.1-SNAPSHOT",
  "com.propensive" % "rapture-io" % "0.7.2",
  "org.typelevel" %% "machinist" % "0.3.0"
)

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}


pomExtra := (
  <scm>
    <url>git@github.com:denisrosset/ariadne.git</url>
    <connection>scm:git:git@github.com:denisrosset/ariadne.git</connection>
  </scm>
  <developers>
    <developer>
      <id>denisrosset</id>
      <name>Denis Rosset</name>
      <url>http://denisrosset.com</url>
    </developer>
  </developers>
)
