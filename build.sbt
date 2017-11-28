name := "robowar"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.6",
  "com.apple" % "AppleJavaExtensions" % "1.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)
