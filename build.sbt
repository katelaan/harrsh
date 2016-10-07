
name := "slex"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.4"

<<<<<<< HEAD
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
=======
>>>>>>> e812e170355888a34e8a0b9ce63098b9a95a3fc5
libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "ch.qos.logback" %  "logback-classic" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"