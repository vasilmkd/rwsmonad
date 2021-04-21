name := "rwsmonad"

scalaVersion := "3.0.0-RC3"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "2.6.0" % Test,
  "org.typelevel" %% "discipline-munit" % "1.0.8" % Test
)
