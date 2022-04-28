package com.evolution.casino.boot

import akka.actor.ActorSystem
import com.evolution.casino.updater._
import com.evolution.casino.{
  CasinoConfig,
  CasinoController,
  CasinoRepository,
  CasinoService
}

class CasinoModule {
  private val system: ActorSystem = ActorSystem("System")
  private val httpClient = new HttpClient
  private val config: HttpConfig = HttpConfig("http://localhost")

  private val casinoRepository = new CasinoRepository
  private val casinoService =
    new CasinoService(casinoRepository, CasinoConfig())

  val casinoController = new CasinoController(casinoService)

//  private val httpServer = new HttpServer(
//    casinoController,
//  )
//  httpServer.start()

  private val appHttpClient = new CasinoHttpClient(
    casinoService,
    httpClient,
    config,
    new JsonParser
  )
  val scheduledUpdates =
    new ScheduledUpdates(appHttpClient, casinoService, system)

//  scheduledUpdates.start()
}
