name := "config"

version := "0.1.0"

organization := "com.twitter"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.21",
  "com.lihaoyi" %% "fastparse" % "0.3.7",
  "ch.qos.logback" % "logback-classic" % "1.1.7" % Test,
	"org.scalatest" %% "scalatest" % "2.2.4" % Test,
  "org.scalacheck" %% "scalacheck" % "1.12.1" % Test
)