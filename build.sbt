scalaVersion := "2.12.5"
version := "1.0"
//scalacOptions += "-Xprint:typer"

val kindProjector = "org.spire-math" %% "kind-projector" % "0.9.6"

lazy val macros = (project in file("macros")).settings(
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "io.frees" %% "iotaz-core"  % "0.3.6"
  ),
  addCompilerPlugin(kindProjector)
)

lazy val root = (project in file(".")).settings(
  addCompilerPlugin(kindProjector),
  scalacOptions += "-Xprint:typer"
).dependsOn(macros).aggregate(macros)
