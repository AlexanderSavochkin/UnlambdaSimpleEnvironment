import sbt._

object MyBuild extends Build {

  lazy val root = Project(id = "root", base = file(".")) aggregate(core, gui)

  lazy val core: Project = Project(id = "core", base = file("Unlambda-Core") )

  lazy val gui: Project = Project(id = "gui", base = file("Unlambda-GUI")) dependsOn(core)

}
