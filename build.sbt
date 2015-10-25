name := "ScalaMCMC"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-language:implicitConversions", "-language:postfixOps")

val breezeVersion = "0.12-SNAPSHOT"

resolvers ++= Seq( //needed for Breeze 0.12-SNAPSHOT
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % breezeVersion,
  "org.scalanlp" %% "breeze-natives" % breezeVersion
)