scalaVersion := "2.11.6"

organization := "SANDEC"

resolvers += Resolver.url("SANDEC", url("http://sandec.de/repo/"))(Resolver.ivyStylePatterns)

libraryDependencies += "SANDEC" %% "simplefx" % "2.1.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:_")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

fork := true