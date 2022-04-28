package com.evolution.user

import com.evolution.infrastructure.http.Controller
import com.evolution.user.UserController.UserDto
import com.evolution.user.domain._
import com.evolution.user.group.GroupController.GroupDto
import com.evolution.user.permission.PermissionController.PermissionDto
import com.evolution.user.report.ReportId

import java.util.UUID

class UserController(
    userService: UserService,
) extends Controller {
  // get: api/user/$userId
  def userById(id: String): Option[UserDto] =
    userService.userById(UserId(id)).map(UserDto.from)

  // get: api/user/$userId/permissions
  def userPermissions(id: String): List[PermissionDto] =
    userService.userById(UserId(id)).toList.flatMap { user =>
      userService.userPermissions(user).map(PermissionDto.from)
    }

  // get: api/user/$userId/groups
  def userGroups(id: String): List[GroupDto] =
    userService.userById(UserId(id)).toList.flatMap { user =>
      userService.userGroups(user).map(GroupDto.from)
    }

  // post: api/user
  def addUser(user: UserDto): Unit =
    userService.addUser(
      User(
        UserId(UUID.randomUUID().toString),
        UserLogin(user.login),
        UserFirstName(user.name),
        UserLastName(user.lastName)
      )
    )

  // get: api/user/$userId/report/$reportId
  def report(userId: String, reportId: String): Option[_] =
    userService.userById(UserId(userId)).flatMap { user =>
      userService.report(user, ReportId(reportId))
    }

  // get: api/user/$userId/analysis
  def analysis(userId: String): Option[_] =
    userService.userById(UserId(userId)).flatMap { user =>
      userService.analysis(user)
    }
}

object UserController {
  final case class UserDto(login: String, name: String, lastName: String)

  object UserDto {
    def from(user: User): UserDto =
      apply(user.login.value, user.firstName.value, user.lastName.value)
  }
}
