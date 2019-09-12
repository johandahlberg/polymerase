enablePlugins(JavaAppPackaging)

import Dependencies._

ThisBuild / scalaVersion := "2.13.0"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "se.scilifelab"
ThisBuild / organizationName := "scilifelab"

lazy val root = (project in file("."))
  .settings(
    name := "polymerase",
    libraryDependencies ++= Seq(scalaTest % Test, scalaCheck % Test)
  ) //dependsOn reedsolomon

//lazy val reedsolomon = RootProject(
//  uri("git://github.com/johandahlberg/reedsolomon-scala.git")
//)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
