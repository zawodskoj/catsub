name := "catsub"
maintainer := "zawodskoj"

version := "0.1"

scalaVersion := "3.2.0"

libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.8.2"
libraryDependencies += "com.softwaremill.sttp.client3" %% "async-http-client-backend-fs2" % "3.8.2"
libraryDependencies += "com.softwaremill.sttp.client3" %% "zio-json" % "3.8.2"

enablePlugins(JavaAppPackaging)
enablePlugins(JDebPackaging)
enablePlugins(DebianPlugin)