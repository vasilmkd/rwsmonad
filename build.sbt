ThisBuild / scalaVersion := "3.0.2"
ThisBuild / githubWorkflowJavaVersions := Seq("adoptium@11")
ThisBuild / githubWorkflowEnv += ("JABBA_INDEX" -> "https://github.com/vasilmkd/jdk-index/raw/main/index.json")
ThisBuild / githubWorkflowTargetBranches := Seq("main")
ThisBuild / githubWorkflowUseSbtThinClient := false
ThisBuild / githubWorkflowPublishTargetBranches := Seq()

ThisBuild / libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "2.6.1" % Test,
  "org.typelevel" %% "discipline-munit" % "1.0.9" % Test
)
