name := "toy-zio"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  scalaOrganization.value % "scala-reflect"  % scalaVersion.value,
  scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided"
)

scalacOptions ++= Seq("-Ymacro-annotations", "-language:experimental.macros")
