name := "toy-zio"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions ++= Seq("-Ymacro-annotations", "-language:experimental.macros", "-Ymacro-debug-lite")

lazy val macros = (project in file("macros")).settings(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-reflect"  % scalaVersion.value,
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided"
  )
)

lazy val root = (project in file("core")).dependsOn(macros)
