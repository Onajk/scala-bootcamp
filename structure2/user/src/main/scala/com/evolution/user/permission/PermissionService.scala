package com.evolution.user.permission

import com.evolution.user.UserRepository
import com.evolution.user.permission.domain.Permission

class PermissionService(userRepository: UserRepository) {
  def addPermission(value: Permission): Unit =
    userRepository.addPermission(value)
  def deletePermission(value: Permission): Unit =
    userRepository.deletePermission(value)
}
