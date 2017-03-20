name := "term-rewriting"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "3.0.1"
)

mainClass in assembly := Some("com.cristis.Main")
