name := "ScalaLabs-solutions"

version := "1.0"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

// You should be able to use the following to read all dependencies from the pom.xml file, but somehow those aren't picked up.
// see: https://github.com/harrah/xsbt/wiki/Library-Management
// externalPom()

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.2.0" % "test",
	"junit" % "junit" % "4.7" % "test",
  "com.typesafe" % "config" % "0.4.0"
)

