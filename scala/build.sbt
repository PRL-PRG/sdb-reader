val scalametaVersion = "4.0.0-M8"

scalaVersion := "2.12.6"
name := "sdb-reader"
organization := "cz.cvut.fit.prl.scala.sdbreader"

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

  "org.scalatest" %% "scalatest" % "3.2.0-SNAP10" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.5" % Test
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions ++= Seq(
  "-deprecation", "-feature", "-unchecked", "-Xfatal-warnings"
)

parallelExecution.in(Test) := false // hello, reflection sync!!

exposePaths("tests", Test)

def exposePaths(projectName: String, config: Configuration) = {
  def uncapitalize(s: String) =
    if (s.length == 0) ""
    else {
      val chars = s.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    }
  val prefix = "sbt.paths." + projectName + "." + uncapitalize(config.name) + "."
  Seq(
    scalacOptions.in(config) := {
      val defaultValue = scalacOptions.in(config).value
      System.setProperty(prefix + "options", defaultValue.mkString(" "))
      defaultValue
    },
    sourceDirectory.in(config) := {
      val defaultValue = sourceDirectory.in(config).value
      System.setProperty(prefix + "sources", defaultValue.getAbsolutePath)
      defaultValue
    },
    resourceDirectory.in(config) := {
      val defaultValue = resourceDirectory.in(config).value
      System.setProperty(prefix + "resources", defaultValue.getAbsolutePath)
      defaultValue
    },
    fullClasspath.in(config) := {
      val defaultValue = fullClasspath.in(config).value
      val classpath = defaultValue.files.map(_.getAbsolutePath)
      System.setProperty(prefix + "classes", classpath.mkString(java.io.File.pathSeparator))
      System.setProperty(
        "sbt.paths.semanticdb-scalac-plugin.compile.jar",
        System.getProperty("user.home") +
          "/.ivy2/cache/org.scalameta/semanticdb-scalac_"+ scalaVersion.value +
          "/jars/" +
          "semanticdb-scalac_" + scalaVersion.value + "-" + scalametaVersion + ".jar"
      )
      defaultValue
    }
  )
}
