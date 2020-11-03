


name := "utils"
organization in ThisBuild := "de.bwhc"
scalaVersion in ThisBuild := "2.13.1"
version in ThisBuild := "1.0-SNAPSHOT"


//-----------------------------------------------------------------------------
// PROJECT
//-----------------------------------------------------------------------------

lazy val root = project.in(file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest"          %% "scalatest"                   % "3.1.1" % Test,
      "org.slf4j"              %  "slf4j-api"                   % "1.7.26",
      "com.chuusai"            %% "shapeless"                   % "2.3.3",
      "com.github.andyglow"    %% "scala-jsonschema"            % "0.5.0",
      "com.github.andyglow"    %% "scala-jsonschema-cats"       % "0.5.0",
      "com.github.andyglow"    %% "scala-jsonschema-play-json"  % "0.5.0"
   )
 )


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings

lazy val compilerOptions = Seq(
  "-encoding", "utf8",
  "-unchecked",
  "-Xfatal-warnings",
  "-feature",
//  "-language:existentials",
  "-language:higherKinds",
//  "-language:implicitConversions",
  "-language:postfixOps",
  "-deprecation"
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )
)

