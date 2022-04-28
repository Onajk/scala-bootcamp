package com.evolution.user.permission

import com.evolution.infrastructure.http.Controller
import com.evolution.user.permission.PermissionController.PermissionDto
import com.evolution.user.permission.domain.{Permission, PermissionName}

class PermissionController(permissionService: PermissionService)
    extends Controller {
  // post: api/permission
  def addPermission(perm: PermissionDto): Unit =
    permissionService.addPermission(Permission(PermissionName(perm.name)))

  // delete: api/permission
  def deletePermission(perm: PermissionDto): Unit =
    permissionService.deletePermission(Permission(PermissionName(perm.name)))
}

object PermissionController {
  final case class PermissionDto(name: String)

  object PermissionDto {
    def from(perm: Permission): PermissionDto = apply(perm.name.value)
  }
}
