resolvers += Resolver.url("SANDEC", url("http://dl.bintray.com/sandec/repo"))(Resolver.ivyStylePatterns)

addSbtPlugin("SANDEC" % "simplefx-plugin" % "2.2.0-SNAPSHOT")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.12.0")
addSbtPlugin("SANDEC" % "javafxmobile-sbt" % "0.1.3")
