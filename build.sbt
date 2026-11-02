ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.8"

lazy val root = (project in file("."))
  .settings(
    name := "fp_lab1",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.4.3"
  )