ThisBuild / scalaVersion := "2.13.8"

ThisBuild / version := "1.1"

//lazy val root = project
//  .in(file("."))
//  .settings(
//    name := "App Structure",
//    normalizedName := "app-structure",
//    libraryDependencies ++= Dependencies.All,
//    Test / testForkedParallel := true,
//    Compile / packageDoc / publishArtifact := false,
//    Compile / doc / sources := Seq.empty,
//    Test / publishArtifact := false
//  )
//  .enablePlugins(DockerPlugin, JavaServerAppPackaging)
//  .aggregate(domain, api) // `sbt test` will execute tests of root + domain and api modules
//
//lazy val api = project
//  .enablePlugins(DockerPlugin, JavaServerAppPackaging)
//  .dependsOn(domain, utils) // transitive dependencies
//
//lazy val domain = project
//  .settings(libraryDependencies ++= Seq(Dependencies.PlayJson))
//
//lazy val utils = project

/*
// packaged by type application

lazy val app = project
  .enablePlugins(DockerPlugin)
  .aggregate(controllers, services, repos, domain, infra, dto)
  .dependsOn(controllers)

lazy val controllers = project
  .dependsOn(domain, infra, services, dto)

lazy val services = project
  .dependsOn(repos, infra)

lazy val repos = project
  .dependsOn(domain, infra)

lazy val domain = project
lazy val infra = project
lazy val dto = project

 */

// packaged by feature application

lazy val app = project
  .in(file(".")) // root
  .enablePlugins(DockerPlugin, JavaServerAppPackaging)
  .aggregate(user, casino, infra)
// .dependsOn(user, group, permission, casino)
//  .dependsOn(user, casino)

lazy val backoffice = project
  .dependsOn(userApi, casinoApi)

lazy val user = project
  .dependsOn(infra, userApi)

lazy val userApi = project

lazy val casino = project
  .dependsOn(infra, casinoApi)

lazy val casinoApi = project

lazy val infra = project
  .settings(
    libraryDependencies ++= Dependencies.All
  )
