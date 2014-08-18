name := "Kaggle Competition"

version := "0.1"

scalaVersion := "2.10.4"

resolvers ++= Seq(
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Akka Repository" at "http://repo.akka.io/releases/"
)

libraryDependencies ++= Seq(
    "org.scalanlp" %% "chalk" % "1.3.2" exclude ("com.typesafe.sbt", "sbt-pgp"),
    "com.typesafe.slick" %% "slick" % "2.0.2",
    "org.slf4j" % "slf4j-nop" % "1.6.4",
    "com.h2database" % "h2" % "1.3.166",
    "mysql" % "mysql-connector-java" % "5.1.25",
    "com.github.nscala-time" %% "nscala-time" % "1.2.0",
    "edu.stanford.nlp" % "stanford-corenlp" % "1.2.0"
)

scalacOptions += "-feature"
