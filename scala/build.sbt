val scalametaVersion = "4.0.0-M8"

ThisBuild / scalaVersion := "2.12.6"
ThisBuild / organization := "cz.cvut.fit.prl.scala.sdbreader"

lazy val hello = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "sdb-reader",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % scalametaVersion,
      "org.scalameta" %% "testkit" % scalametaVersion,
      "org.scalameta" %% "metap" % scalametaVersion,
      "org.scalameta" %% "metacp" % scalametaVersion,
      "org.scalameta" %% "symtab" % scalametaVersion,
      "org.scalameta" %% "semanticdb" % scalametaVersion,
      "org.scalameta" % ("semanticdb-scalac_" + scalaVersion.value) % scalametaVersion,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,

      "com.github.pathikrit" %% "better-files" % "3.4.0",
      "com.github.scopt" % "scopt_2.11" % "3.7.0",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "com.nrinaudo" %% "kantan.csv-generic" % "0.4.0",

      "org.scalatest" %% "scalatest" % "3.2.0-SNAP10" % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.5" % Test
    ),
    buildInfoPackage := "cz.cvut.fit.prl.scala.implicits",
    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      scalaVersion,
      sbtVersion,
      "scalametaVersion" -> scalametaVersion,
      scalacOptions.in(Test),
      // fullClasspath is impossible since it will need also to recompile the actual BuildInfo
      externalDependencyClasspath.in(Test),
      "semanticdbScalacPluginJar" -> (
        System.getProperty("user.home") +
        "/.ivy2/cache/org.scalameta/semanticdb-scalac_"+ scalaVersion.value +
        "/jars/" +
        "semanticdb-scalac_" + scalaVersion.value + "-" + scalametaVersion + ".jar"
      )
    ),
    scalacOptions ++= Seq(
      "-deprecation", "-feature", "-unchecked", "-Xfatal-warnings"
    ),
    parallelExecution.in(Test) := false, // hello, reflection sync!!
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  )
