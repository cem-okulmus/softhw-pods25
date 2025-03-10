ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "rewrite",
    assembly / mainClass := Some("QueryRewriter")
  )

mainClass in Compile := Some("QueryRewriter")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}

// https://mvnrepository.com/artifact/org.apache.spark/spark-sql
libraryDependencies += "org.apache.spark" %% "spark-sql" % "3.5.1"
// https://mvnrepository.com/artifact/org.apache.calcite/calcite-core
libraryDependencies += "org.apache.calcite" % "calcite-core" % "1.36.0"
// https://mvnrepository.com/artifact/org.apache.calcite/calcite-csv
libraryDependencies += "org.apache.calcite" % "calcite-csv" % "1.36.0"
// https://mvnrepository.com/artifact/org.postgresql/postgresql
libraryDependencies += "org.postgresql" % "postgresql" % "42.7.2"
// https://mvnrepository.com/artifact/com.lihaoyi/upickle
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.0.0-M2"

