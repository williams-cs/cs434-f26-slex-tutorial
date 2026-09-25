name := "slex-tutorial"

scalaVersion := "3.3.6"

libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test

// Spec files: assemble the .slex action blocks at build time (Path A;
// see project/SpecGen.scala), and keep the editor wiring current on
// every sbt load.
Compile / sourceGenerators += Def.task {
  scup.SpecGen.generateAll(
    baseDirectory.value, (Compile / sourceManaged).value / "specgen")
}.taskValue

Global / onLoad := (Global / onLoad).value.andThen { s =>
  scup.VscodeSetup(new java.io.File(".")); s
}

// Spec paths in the demos and tests are project-root relative;
// forked tests always run in the project root.
Test / fork := true
