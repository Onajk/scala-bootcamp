package com.evolution.user.group

import com.evolution.user.UserRepository
import com.evolution.user.group.domain.Group

class GroupService(userRepository: UserRepository) {
  def addGroup(group: Group): Unit = userRepository.addGroup(group)
  def deleteGroup(group: Group): Unit = userRepository.deleteGroup(group)
}
