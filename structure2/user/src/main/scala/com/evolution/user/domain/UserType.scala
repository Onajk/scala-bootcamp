package com.evolution.user.domain

sealed trait UserType
object UserType {
  case object Operation extends UserType
  case object Player extends UserType
}
