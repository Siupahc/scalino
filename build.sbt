ThisBuild / scalaVersion := "3.10.1"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalino",
    scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:all"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    // `run` calls sys.exit with the exit code of the program, so it gets its own JVM.
    run / fork := true,
  )
