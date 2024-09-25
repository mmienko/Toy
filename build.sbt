ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name                                         := "Toy",
    libraryDependencies += "com.disneystreaming" %% "weaver-cats" % "0.8.3" % Test,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )
