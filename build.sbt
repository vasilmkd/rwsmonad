name := "rwsmonad"

scalaVersion := "3.0.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "2.6.1" % Test,
  "org.typelevel" %% "discipline-munit" % "1.0.9" % Test
)
