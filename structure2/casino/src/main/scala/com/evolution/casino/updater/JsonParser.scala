package com.evolution.casino.updater

import com.evolution.casino.domain.Casino
import com.evolution.casino.domain.CasinoJson._
import play.api.libs.json.Json

class JsonParser {
  def parse(value: String): Option[Casino] =
    Json.parse(value).asOpt[Casino]

  def parseList(value: String): List[Casino] = {
    Utils.logDebug(value)
    Utils.validate(value).toList.flatMap { validated =>
      Json.parse(validated).as[List[Casino]]
    }
  }
}
