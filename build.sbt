name := "matcher"

version := "0.1"

scalaVersion := "2.12.13"

val csvParserVersion = "1.0.3"
val logbackClassicVersion = "1.2.3"
val scalaLoggingVersion = "3.9.2"

val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion
val csvParser = "de.siegmar" % "fastcsv" % csvParserVersion
val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackClassicVersion

libraryDependencies ++= Seq(csvParser, scalaLogging, logbackClassic)
