name := "GrowCode"

version := "1.0"

scalaVersion := "2.9.1"
  
scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.2.0"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.2.0"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.1"

resolvers += "scala-tools" at "http://www.scala-tools.org/repo-releases"

libraryDependencies += "org.rogach" %% "scallop" % "0.3.9"

libraryDependencies += "org.clapper" %% "argot" % "0.3.5"

resolvers += "librob" at "http://rob-p.github.com/scala-utils"

libraryDependencies += "net.robpatro" %% "scala-utils" % "1.0.0"
