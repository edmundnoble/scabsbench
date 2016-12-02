name in ThisBuild := "scolls"

scalaVersion in ThisBuild := "2.11.8"

lazy val scolls = project.in(file("."))
  .dependsOn(coreJVM, coreJS, bench)
  .aggregate(coreJVM, coreJS, bench)

lazy val core = crossProject.in(file("core"))

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

enablePlugins(JmhPlugin)

lazy val bench = project.in(file("bench"))
  .settings(libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2" % Test)
  .settings(testFrameworks += new TestFramework(
    "org.scalameter.ScalaMeterFramework"))
  .dependsOn(coreJVM)
