name := "BinaryCookie"
version := "0.1"

scalaVersion := "2.12.8"
scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.5.0",
  "com.chuusai"   %% "shapeless" % "2.3.3"
)