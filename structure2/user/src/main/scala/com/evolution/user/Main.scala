package com.evolution.user

import com.evolution.infrastructure.http.HttpServer

object Main extends App {
  val user = new UserModule
  import user._

  val httpServer = new HttpServer(
    assignmentController,
    userController,
    groupController,
    permissionController,
  )

  httpServer.start()
}
