ThisBuild / scalaVersion := "3.1.0"
ThisBuild / githubWorkflowJavaVersions := Seq("adoptium@17")
ThisBuild / githubWorkflowEnv += ("JABBA_INDEX" -> "https://github.com/typelevel/jdk-index/raw/main/index.json")
ThisBuild / githubWorkflowTargetBranches := Seq("main")
ThisBuild / githubWorkflowUseSbtThinClient := false
ThisBuild / githubWorkflowPublishTargetBranches := Seq()

ThisBuild / libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "2.6.1" % Test,
  "org.typelevel" %% "discipline-munit" % "1.0.9" % Test
)
