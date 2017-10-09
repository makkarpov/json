organization := "ru.makkarpov"
name := "pray-json"
description := "play-json as it was supposed to be"
version := "0.3"

scalaVersion := "2.11.8"
crossScalaVersions := Seq("2.11.8", "2.12.1")

libraryDependencies := Seq(
  "com.typesafe.play" %% "play-json" % "2.6.2",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
homepage := Some(url(s"https://github.com/makkarpov/${normalizedName.value}"))
organizationName := "Maxim Karpov"
organizationHomepage := Some(url("https://github.com/makkarpov"))
scmInfo := Some(ScmInfo(
  browseUrl = url(s"https://github.com/makkarpov/${normalizedName.value}"),
  connection = s"scm:git://github.com/makkarpov/${normalizedName.value}.git"
))

pomExtra := {
  <developers>
    <developer>
      <id>makkarpov</id>
      <name>Maxim Karpov</name>
      <url>https://github.com/makkarpov</url>
    </developer>
  </developers>
}

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}