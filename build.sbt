name := "ScalaLabs-solutions"

version := "1.0"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

// You should be able to use the following to read all dependencies from the pom.xml file, but somehow those aren't picked up.
// see: https://github.com/harrah/xsbt/wiki/Library-Management
// externalPom()
//resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	//"org.scalatest" %% "scalatest" % "2.2.0" % "test",
	//"junit" % "junit" % "4.7" % "test",
  //"org.jliszka" % "probability-monad_2.9.2" % "1.0.0",
	"commons-io" % "commons-io" % "2.4",
  "org.mongodb" %% "casbah" % "2.7.3",
  "com.typesafe" % "config" % "0.4.0",
  //"com.typesafe.akka" % "akka-actor_2.11" % "2.3.7",
  "org.reactivemongo" %% "reactivemongo" % "0.10.5.0.akka23"
)

