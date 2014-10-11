import _root_.sbt.Keys._

name := "Schema17"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.4.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"