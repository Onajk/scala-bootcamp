package com.evolution.user.domain

final case class Player(user: User) {
  val utype: UserType = UserType.Player
}
