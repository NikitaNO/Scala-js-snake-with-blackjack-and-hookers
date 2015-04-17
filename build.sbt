enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"

libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0"

libraryDependencies += "com.lihaoyi" %% "scalarx" % "0.2.8"

libraryDependencies += "com.lihaoyi" %%% "scalarx" % "0.2.8"

name := "Scala.js Tutorial"

scalaVersion := "2.11.5" // or any other Scala version >= 2.10.2

skip in packageJSDependencies := false