name := "scolls"

scalaVersion := "2.11.8"

lazy val root = project.in(file("."))

lazy val core = crossProject.in(file("core"))

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
