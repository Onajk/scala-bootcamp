package com.evolution.user.group

import com.evolution.infrastructure.http.Controller
import com.evolution.user.group.GroupController.GroupDto
import com.evolution.user.group.domain.{Group, GroupName}

class GroupController(groupService: GroupService) extends Controller {
  // post: api/group
  def addGroup(group: GroupDto): Unit =
    groupService.addGroup(Group(GroupName(group.name)))

  // delete: api/group
  def deleteGroup(group: GroupDto): Unit =
    groupService.deleteGroup(Group(GroupName(group.name)))
}

object GroupController {
  import com.evolution.user.group.domain.Group

  final case class GroupDto(name: String)

  object GroupDto {
    def from(group: Group): GroupDto = apply(group.name.value)
  }
}
