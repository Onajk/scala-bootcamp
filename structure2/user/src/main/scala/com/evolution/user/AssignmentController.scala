package com.evolution.user

import com.evolution.infrastructure.http.Controller
import com.evolution.user.group.domain.GroupName
import com.evolution.user.permission.domain.PermissionName

class AssignmentController(
    userService: UserService,
) extends Controller {
  // post: api/group/$groupId/permissions
  def assignPermissions(groupId: String, permissionIds: List[String]): Unit =
    userService.assignPermissions(
      GroupName(ExtractId(groupId)),
      permissionIds.map(PermissionName)
    )

  // post: api/user/$userId/groups
  def assignGroups(userId: String, groupIds: List[String]): Unit = ()
}

object ExtractId {
  def apply(value: String): String = value
}
