ThisBuild / version := "0.3.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(GraalVMNativeImagePlugin)
  .settings(
    name := "tableaux",
    idePackagePrefix := Some("pl.wojciechkarpiel.tableaux"),
    Compile / mainClass := Some("pl.wojciechkarpiel.tableaux.app.Main"),
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.4.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test",
    graalVMNativeImageOptions ++= Seq(
      "--initialize-at-build-time",
      "--link-at-build-time",
      "--no-fallback",
      "--static",
    ),
  )

Global / excludeLintKeys += idePackagePrefix
