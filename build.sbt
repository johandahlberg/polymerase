enablePlugins(JavaAppPackaging)

import Dependencies._

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "se.scilifelab"
ThisBuild / organizationName := "scilifelab"
ThisBuild / maintainer := "johan.dahlberg@medsci.uu.se"

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

logBuffered in Test := false

lazy val root = (project in file("."))
  .settings(
    name := "polymerase",
    libraryDependencies ++= Seq(
      scalaLogging,
      logBack,
      scalaTest % Test,
      scalaCheck % Test,
      scalaTestScalaCheckPlugin % Test
    )
  )
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
