name := "game-engine-2d"
organization := "org.kcw"
version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  // These will be moved into the build file for an application, at some point
  "com.typesafe" % "config" % "1.3.1",
  "com.apple" % "AppleJavaExtensions" % "1.4",

  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")
