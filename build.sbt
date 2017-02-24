
name := "harrsh"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-Xmax-classfile-name","78")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "ch.qos.logback" %  "logback-classic" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.9"