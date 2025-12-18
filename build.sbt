val scala3Version = "3.7.4"

lazy val root = project
  .in(file("."))
  .settings(
    name                                       := "litmusgraph",
    version                                    := "0.1.0-SNAPSHOT",
    scalaVersion                               := scala3Version,
    libraryDependencies += "com.github.j-mie6" %% "parsley"   % "5.0.0-M15",
    libraryDependencies += "com.lihaoyi"       %% "pprint"    % "0.9.6",
    libraryDependencies += "org.scalactic"     %% "scalactic" % "3.2.19" % Test,
    libraryDependencies += "org.scalatest"     %% "scalatest" % "3.2.19" % Test,
  )
