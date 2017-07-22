def v: String = "3.5.0-SNAPSHOT"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(Seq(
      version := v,
      organization := "com.github.scopt",
      scalaVersion := "2.11.8",
      crossScalaVersions := Seq("2.11.8", "2.10.6", "2.12.0-M4"),
      homepage := Some(url("https://github.com/scopt/scopt")),
      licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))
    )),
    name := "scopt",
    // site
    // to preview, preview-site
    // to push, ghpages-push-site
    // site.settings,
    // site.includeScaladoc(s"$v/api"),
    // ghpages.settings,
    // git.remoteRepo := "git@github.com:scopt/scopt.git",
    description := """a command line options parsing library""",
    libraryDependencies ++= {
      scalaVersion.value match {
        case x if x startsWith "2.10." => List("org.specs2" %% "specs2" % "2.3.3" % "test")
        case x if x startsWith "2.11." => List("org.specs2" %% "specs2" % "2.3.11" % "test")
        case _ => Nil
      }
    },
    scalacOptions ++= Seq("-language:existentials"),
    resolvers += "sonatype-public" at "https://oss.sonatype.org/content/repositories/public",
    // scaladoc fix
    unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))
  )
