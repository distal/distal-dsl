name := "DiStaL"

organization := "ch.epfl.lsr"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

// scalacOptions += "-Ymacro-debug-lite"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

libraryDependencies += "ch.epfl.lsr" %% "lsr-netty-protocols-base" % "0.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.1"

fork := true
