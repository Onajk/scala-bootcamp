package com.evolution.casino

import com.evolution.casino.domain.{Casino, CasinoId, CasinoName}
import com.evolution.casino.infrastructure.AbstractRepository

import java.time.Instant

class CasinoRepository extends AbstractRepository {
  def lastUpdated: Instant = Instant.now()
  def casinos: List[Casino] = Nil
  def casinoById(id: String): Option[Casino] =
    Some(Casino(CasinoId(id), CasinoName("Test")))
  def save(casino: Casino): Unit = ()
}
