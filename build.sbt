name := "CompilerProject"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0"

scalaSource in Compile := file("/Users/stylejy/Documents/King's Project/CompilerProject/core")

scalaSource in Test := file("/Users/stylejy/Documents/King's Project/CompilerProject/test")