package com.evolution.casino.updater

import com.evolution.casino.CasinoService

class CasinoHttpClient(
    service: CasinoService,
    http: HttpClient,
    conf: HttpConfig,
    jsonParser: JsonParser
) {
  def retrieveCasinos = {
    val response = http.post(Request(service.lastUpdated.toString, conf.url))
    val casinos = jsonParser.parseList(response.body)
    Utils.log(s"retrieved casinos: $casinos")
    casinos
  }
}

final case class HttpConfig(url: String)
