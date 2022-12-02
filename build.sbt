ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file(".")).settings(
  name := "advent-of-code-2022",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.9.0",
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.3.12",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.3.12",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel"       %% "cats-effect-std" % "3.3.12",
    "com.disneystreaming" %% "weaver-cats"     % "0.8.1" % Test,
  )
)
