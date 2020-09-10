import Dependencies._
import Settings._

lazy val goals = (project in file("goals")).
  settings(Settings.settings: _*).
  settings(Settings.goalsSettings: _*).
  configs(Test)

