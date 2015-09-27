import sbtassembly.AssemblyPlugin.defaultShellScript

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation",
                      "-Ybackend:GenBCode", "-Ydelambdafy:method")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
)

assemblyOption in assembly := (assemblyOption in assembly).value.copy(
  prependShellScript = Some(defaultShellScript)
)

assemblyJarName in assembly := s"${name.value}"
