import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "hKrish"
  val buildVersion      = "0.1.2"
  val buildScalaVersion = "2.9.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt
    )
}

// Shell prompt which show the current project, 
// git branch and build version
object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }
  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
      getOrElse "-" stripPrefix "## "
  )

  val buildShellPrompt = { 
    (state: State) => {
      val currProject = Project.extract (state).currentProject.id
      "%s:%s:%s> ".format (
        currProject, currBranch, BuildSettings.buildVersion
      )
    }
  }
}

object holmes extends Build {
  import BuildSettings._

  lazy val holmes = Project(id = "holmes",
                          base = file("."),
                          settings = buildSettings) aggregate(crawl, search, wavelet)

  lazy val crawl = Project(id = "holmes-crawl",
                         base = file("crawl")) dependsOn(wavelet)

  lazy val search = Project(id = "holmes-search",
                         base = file("search")) dependsOn(wavelet)

  lazy val wavelet = Project(id = "holmes-wavelet",
                         base = file("wavelet"))
}
