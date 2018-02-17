import sbt._
import Keys._

object SonatypePlugin extends AutoPlugin {
  override def requires = sbt.plugins.JvmPlugin
  override def trigger = allRequirements

  override def projectSettings: Seq[Setting[_]] = Seq(
    pomExtra :=
      (<developers>
        <developer>
          <id>eed3si9n</id>
          <name>Eugene Yokota</name>
          <url>http://eed3si9n.com</url>
        </developer>
      </developers>),

    // --- Sonatype settings ---

    publishMavenStyle := true,

    publishArtifact in (Compile, packageBin) := true,

    publishArtifact in Test := false,

    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots") 
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },

    pomIncludeRepository := { x => false }

  )

}