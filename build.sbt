name := "rwsmonad"

scalaVersion := "3.0.0-RC2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "2.5.0" % Test,
  "org.typelevel" %% "discipline-munit" % "1.0.7" % Test
)
