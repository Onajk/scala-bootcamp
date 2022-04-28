package com.evolution.casino.boot

import com.evolution.infrastructure.http.HttpServer

object Main extends App {
  val casino = new CasinoModule
  import casino._

  val httpServer = new HttpServer(
    casinoController,
  )

  httpServer.start()
  scheduledUpdates.start()
}
