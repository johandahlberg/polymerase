enablePlugins(JavaAppPackaging)

import Dependencies._

scalaVersion := "2.13.2"
version := "0.1.0-SNAPSHOT"
organization := "se.scilifelab"
organizationName := "scilifelab"
maintainer := "johan@uppsala-bioinformatics.se"

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
