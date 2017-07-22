
pomExtra :=
  (<scm>
    <url>git@github.com:scopt/scopt.git</url>
    <connection>scm:git:git@github.com:scopt/scopt.git</connection>
  </scm>
  <developers>
    <developer>
      <id>eed3si9n</id>
      <name>Eugene Yokota</name>
      <url>http://eed3si9n.com</url>
    </developer>
  </developers>)

// --- Sonatype settings ---

publishMavenStyle := true

publishArtifact in (Compile, packageBin) := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  val v = version.value
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { x => false }
