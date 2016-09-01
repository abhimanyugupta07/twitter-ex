name := "config"

version := "0.1.0"

organization := "com.twitter"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.3.7",
	"org.scalatest" %% "scalatest" % "2.2.4" % Test,
  "org.scalacheck" %% "scalacheck" % "1.12.1" % Test
)