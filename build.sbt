import sbt.Keys.libraryDependencies

val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "popl2022-assn2",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"

  )
