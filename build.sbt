val Http4sVersion = "0.23.13"
val ScalaTestVersion = "3.2.12"

lazy val root = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    organization := "peschke",
    name := "bulk-caller",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.8",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-client" % Http4sVersion,
      "org.http4s" %% "http4s-ember-client" % Http4sVersion,
      "org.http4s" %% "http4s-circe" % Http4sVersion,
      "io.circe" %% "circe-generic" % "0.14.2",
      "io.circe" %% "circe-parser" % "0.14.2",
      "com.typesafe" % "config" % "1.4.2",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "org.rudogma" %% "supertagged" % "2.0-RC2",
      "io.jvm.uuid" %% "scala-uuid" % "0.3.1",
      "com.beachape" %% "enumeratum" % "1.7.0",
      "ch.qos.logback" % "logback-classic" % "1.2.11" % Runtime,
      "org.typelevel" %% "cats-parse" % "0.3.8",
      "org.typelevel" %% "cats-effect" % "3.3.14",
      "org.typelevel" %% "cats-core"% "2.8.0",
      "com.monovore" %% "decline" % "2.3.0",
      "com.lihaoyi" %% "os-lib" % "0.8.1",
      "org.scalacheck" %% "scalacheck" % "1.16.0" % "test",
      "org.scalatest" %% "scalatest" % ScalaTestVersion % "test",
      "org.scalatest" %% "scalatest-wordspec" % ScalaTestVersion % "test",
      "org.scalatest" %% "scalatest-propspec" % ScalaTestVersion % "test",
      "org.scalatestplus" %% "scalacheck-1-16" % "3.2.12.0" % "test"
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    buildInfoKeys := Seq[BuildInfoKey](name, version),
    buildInfoPackage := "peschke"
  )
