name := "unlambda-gui-in-scala"

version := "0.0.1"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq( "org.scalatest" %% "scalatest" % "1.6.1" % "test" )

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }