scalaVersion := "3.6.2"

scalacOptions ++= Seq("-Wall", "-Wconf:any:e", "-Wunused:all", "-feature", "-deprecation", "-unchecked", "-Yexplicit-nulls")

testFrameworks += new TestFramework("munit.Framework")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "mainargs" % "0.7.6" ,
  "com.lihaoyi" %% "os-lib" % "0.11.3" ,
  "com.lihaoyi" %% "pprint" % "0.9.0" ,
  "com.lihaoyi" %% "sourcecode" % "0.4.2" ,
  "com.lihaoyi" %% "upickle" % "4.0.2" ,
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5" ,
  "org.scala-lang.modules" %% "scala-xml" % "2.3.0" ,
  "org.slf4j" % "slf4j-simple" % "2.0.16" 
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "0.7.29" % Test
)

dependsOn(RootProject(uri("https://github.com/utgheith/rules.git")))

