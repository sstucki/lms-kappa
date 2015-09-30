name := "lms-kappa"

organization := "ch.lamp.epfl"

version := "0.1-SNAPSHOT"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.11.2"

//--- Dependencies

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scala-lang.lms" %% "lms-core" % "1.0.0-SNAPSHOT",
  "org.scala-lang.virtualized" % "scala-compiler" % "2.11.2",
  "org.scala-lang.virtualized" % "scala-library" % "2.11.2",
  "org.scala-lang.virtualized" % "scala-reflect" % "2.11.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.2")

//--- End of Dependencies

// General compiler options
scalacOptions ++= Seq(
  "-deprecation", "-unchecked", "-Xexperimental",
  "-Yvirtualize", "-feature", "-language:higherKinds")

// Documentation (scaladoc) options
scalacOptions in doc += "-external-urls:scala=http://www.scala-lang.org/"

// Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false
