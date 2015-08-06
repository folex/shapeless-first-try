name := """basic"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.5"
)

incOptions := incOptions.value.withNameHashing(false)

