package com.evolution.casino

import com.evolution.casino.CasinoController.CasinoDto
import com.evolution.casino.domain.{Casino, CasinoId, CasinoName}
import com.evolution.infrastructure.http.Controller

class CasinoController(casinoService: CasinoService) extends Controller {
  // get: api/casinos
  def allCasinos: List[CasinoDto] = casinoService.allCasinos.map(CasinoDto.from)

  // post: api/casinos
  def update(casino: CasinoDto): Unit =
    casinoService.casinoById(CasinoId(casino.id)).foreach { c =>
      casinoService.save(
        Casino(c.id, CasinoName(casino.name))
      )
    }
}

object CasinoController {
  final case class CasinoDto(id: String, name: String)

  object CasinoDto {
    def from(casino: Casino): CasinoDto =
      apply(casino.id.value, casino.name.value)
  }
}
