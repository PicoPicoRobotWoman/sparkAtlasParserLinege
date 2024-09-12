version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.10"

name := "sparkAtlasParserLinege"

val sparkVersion = "3.4.0"
val sparkDependencies = Seq(
  "org.apache.spark" %% "spark-core",
  "org.apache.spark" %% "spark-sql",
).map(_ % sparkVersion % Compile)

val postgresVersion = "42.6.0"
val client3Version = "3.8.16"
val circeVersion = "0.14.4"
val json4sVersion = "4.0.6"

val otherDependency = Seq(
  "org.postgresql" % "postgresql" % postgresVersion,
  "com.softwaremill.sttp.client3" %% "core" % client3Version,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion
)


libraryDependencies ++= sparkDependencies ++ otherDependency

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
