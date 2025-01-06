ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "aoclang",
    idePackagePrefix := Some("aoclang")
  )
