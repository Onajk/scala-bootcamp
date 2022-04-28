package com.evolution.user.report

import com.evolution.user.domain.UserId
import com.evolution.user.{AbstractRepository, UserRepository}

class UserAnalysisRepository(repository: UserRepository)
    extends AbstractRepository {
  def analysis(userId: UserId): Option[Analysis] = None
}
