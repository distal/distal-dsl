name := "DiStaL"

organization := "ch.epfl.lsr"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

scalacOptions += "-deprecation"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

//libraryDependencies += "com.typesafe" % "config" % "0.5.0"

libraryDependencies += "ch.epfl.lsr" %% "lsr-netty-protocols-base" % "0.1"

fork := true
