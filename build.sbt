name := "matcher"

version := "0.1"

scalaVersion := "2.12.13"

val csvParserVersion = "1.0.3"
val scoptVersion = "3.7.1"

val csvParser = "de.siegmar" % "fastcsv" % csvParserVersion
val scopt = "com.github.scopt" %% "scopt" % scoptVersion

libraryDependencies ++= Seq(csvParser, scopt)