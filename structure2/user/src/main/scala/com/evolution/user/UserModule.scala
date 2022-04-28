package com.evolution.user

import com.evolution.user.group.{GroupController, GroupService}
import com.evolution.user.permission.{PermissionController, PermissionService}
import com.evolution.user.report.{UserAnalysisRepository, UserReportsRepository}

class UserModule {
  private val userRepository = new UserRepository
  private val userAnalysisRepository = new UserAnalysisRepository(
    userRepository)
  private val userReportsRepository = new UserReportsRepository(userRepository)
  private val userService = new UserService(
    userRepository,
    userAnalysisRepository,
    userReportsRepository,
    UserConfig()
  )
  private val permissionService = new PermissionService(userRepository)
  private val groupService = new GroupService(userRepository)

  val assignmentController = new AssignmentController(userService)
  val userController = new UserController(userService)
  val permissionController = new PermissionController(permissionService)
  val groupController = new GroupController(groupService)

//  private val httpServer = new HttpServer(
//    assignmentController,
//    userController,
//    groupController,
//    permissionController,
//  )
//
//  httpServer.start()
}
