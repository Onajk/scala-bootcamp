package com.evolution.user.report

import com.evolution.user.domain.User

final case class ReportId(value: String) extends AnyVal

final case class Report(user: User)
