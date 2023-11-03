name := "Simple Set Up for Exam Questions"

version := "0.4"

scalaVersion := "2.13.3"

scalacOptions ++= Seq (
  "-Xfatal-warnings", 
  "-Ymacro-annotations",
  "-deprecation", 
  "-feature", 
  "-language:implicitConversions"
)
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" 
libraryDependencies += "org.scalatest" %% "scalatest-freespec" % "3.2.0" 
libraryDependencies += "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.0" 
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.0.0" 

libraryDependencies += "com.github.julien-truffaut" %% "monocle-core"  % "2.0.3"
libraryDependencies += "com.github.julien-truffaut" %% "monocle-macro" % "2.0.3"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0-M2"

initialCommands in console := """
import adpro._
"""
