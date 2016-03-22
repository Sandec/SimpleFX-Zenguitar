enablePlugins(SimpleFXPlugin)
enablePlugins(plugin.JavaFXMobilePlugin)

scalaVersion := "2.11.7"

organization := "SANDEC"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:_")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

fork := true