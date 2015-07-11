import sbt._
import Keys._

object VoiceAPIBuild extends Build {
  lazy val root = Project(
    "voice-api",
    file("."),
    settings = Defaults.coreDefaultSettings ++ Seq(
      organization := "com.pragmaticideal",
      name := "voice-api",
      version := "0.0.1-SNAPSHOT",
      scalaVersion := "2.11.6",
      resolvers += Classpaths.typesafeReleases,
      initialCommands := "import com.pragmaticideal.voiceapi._",
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc(),
        "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc()
      )
    )
  )
}

