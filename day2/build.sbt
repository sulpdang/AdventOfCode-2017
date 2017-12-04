import java.io._

name := "day2"

organization := "com.cafe24.bidulgi329"

version := "0.0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.12" % "test",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
  "org.scalactic" %% "scalactic" % "3.0.4" % "test"
)

unmanagedJars in Compile ++= {
  val base = baseDirectory.value
  val customJars = base / "lib" ** "*.jar"
  customJars.classpath
}

scalacOptions ++= Seq("-deprecation", "-feature")

scalacOptions in Compile ++= {
  Seq("-encoding","UTF8")
}

javacOptions ++= {
  Seq("-encoding","UTF8")
}

