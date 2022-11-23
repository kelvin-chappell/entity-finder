val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "entity-finder",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.apache.opennlp" % "opennlp-tools" % "2.0.0",
      "org.jsoup"          % "jsoup"         % "1.15.3",
      "org.scalameta"     %% "munit"         % "0.7.29" % Test
    )
  )
