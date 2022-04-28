package com.evolution.casino

import com.evolution.casino.domain.{Casino, CasinoId}

import java.time.Instant

class CasinoService(repository: CasinoRepository, config: CasinoConfig) {
  def lastUpdated: Instant = repository.lastUpdated

  def saveAll(casinos: List[Casino]): Unit = casinos.foreach(repository.save)

  def save(casino: Casino): Unit = repository.save(casino)

  def allCasinos: List[Casino] = repository.casinos

  def casinoById(id: CasinoId): Option[Casino] = repository.casinoById(id.value)
}

final case class CasinoConfig(showAllUsers: Boolean = true)
