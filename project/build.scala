import sbt._
import Keys._

import org.scalatra.sbt._

object VoiceAPIBuild extends Build {

  val scalatraVersion = "2.3.0"
  val json4sVersion = "3.2.9"

  lazy val root = Project(
    "voice-api",
    file("."),
    settings = ScalatraPlugin.scalatraWithJRebel ++ Defaults.coreDefaultSettings ++ Seq(
      organization := "com.pragmaticideal",
      name := "voice-api",
      version := "0.0.1-SNAPSHOT",
      scalaVersion := "2.11.6",
      resolvers ++= Seq(Classpaths.typesafeReleases,
            "Sonatype OSS" at "http://oss.sonatype.org/content/repositories/releases/",
            "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snap",
            "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"),
      initialCommands := "import com.pragmaticideal.voiceapi._",
      libraryDependencies ++= Seq(
        // Scalatest
        "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc(),
        "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc(),
        // Scalatra
        "org.scalatra" %% "scalatra" % scalatraVersion withSources() withJavadoc(),
        "org.scalatra" %% "scalatra-scalate" % scalatraVersion withSources() withJavadoc(),
        "org.scalatra" %% "scalatra-json" % scalatraVersion withSources() withJavadoc(),
        // json4s
        "org.json4s" %% "json4s" % json4sVersion,
        "org.json4s" %% "json4s-jackson" % json4sVersion,
        // Jetty
        "org.eclipse.jetty" % "jetty-webapp" % "9.1.5.v20140505" % "container",
        "org.eclipse.jetty" % "jetty-plus" % "9.1.5.v20140505" % "container",
        "javax.servlet" % "javax.servlet-api" % "3.1.0"
      )
    )
  )
}

