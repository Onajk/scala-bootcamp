package com.evolution.casino.updater

import akka.actor.{ActorSystem, Cancellable}
import com.evolution.casino.CasinoService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class ScheduledUpdates(
    client: CasinoHttpClient,
    casinoService: CasinoService,
    system: ActorSystem
) {
  def start(): Unit = {
    val casinosRetrieval: Cancellable =
      system.scheduler.schedule(initialDelay = 1.second, interval = 5.seconds) {
        casinoService.saveAll(client.retrieveCasinos)
      }
    system.scheduler.scheduleOnce(25.seconds) {
      casinosRetrieval.cancel()
      Utils.logDebug("finished casinos retrieval!")
    }
  }
}
