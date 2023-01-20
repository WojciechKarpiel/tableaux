ThisBuild / version := "0.2.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "tableaux",
    idePackagePrefix := Some("pl.wojciechkarpiel.tableaux"),
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.4.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test",
  )

Global / excludeLintKeys += idePackagePrefix
