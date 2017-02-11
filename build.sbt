name in ThisBuild := "scabsbench"

scalaVersion in ThisBuild := "2.11.8"

scalaOrganization in ThisBuild := "org.typelevel"

scalacOptions in ThisBuild += "-Ypartial-unification"

lazy val scabsbench = project.in(file("."))
  .dependsOn(coreJVM, coreJS, bench, test)
  .aggregate(coreJVM, coreJS, bench, test)

lazy val core = crossProject
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(libraryDependencies += "com.chuusai" %%% "shapeless" % "2.3.2")
  .settings(libraryDependencies += "org.atnos" %%% "eff" % "3.0.0")
  .settings(libraryDependencies += "com.github.mpilquist" %%% "simulacrum" % "0.10.0")
  .settings(addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))
  .settings(resolvers += Resolver.sonatypeRepo("releases"))
  .settings(addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"))

resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

enablePlugins(JmhPlugin)

lazy val bench = project.in(file("bench"))
  .settings(libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2")
  .settings(resolvers += Resolver.sonatypeRepo("releases"))
  .settings(parallelExecution in Test := false)
  .settings(addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"))
  .settings(testFrameworks += new TestFramework(
    "org.scalameter.ScalaMeterFramework"))
  .settings(scalacOptions ++= Seq(
      "-language:higherKinds"
  ))
  .dependsOn(coreJVM)

lazy val test = project.in(file("test"))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1")
  .settings(resolvers += Resolver.sonatypeRepo("releases"))
  .settings(addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"))
  .settings(scalacOptions ++= Seq(
      "-language:higherKinds"
  ))
  .dependsOn(coreJVM)
