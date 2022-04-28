package com.evolution.user

import com.evolution.user.domain.{User, UserId}
import com.evolution.user.group.domain.Group
import com.evolution.user.permission.domain.Permission

class UserRepository extends AbstractRepository {
  def userById(id: UserId): Option[User] =
    read[User].filter(_.id == id)

  def userPermissions(id: UserId): List[Permission] = Nil
  def userGroups(id: UserId): List[Group] = Nil

  def addUser(user: User): Unit = add(user)
  def deleteUser(user: User): Unit = delete(user)
  def updateUser(user: User): Unit = userById(user.id).update(user)

  def addPermission(value: Permission): Unit = ()
  def deletePermission(value: Permission): Unit = ()

  def addGroup(value: Group): Unit = ()
  def deleteGroup(value: Group): Unit = ()
}

/*

class GroupRepository extends AbstractRepository {
  def addGroup(value: Group): Unit = ()
  def deleteGroup(value: Group): Unit = ()
}

class PermissionRepository extends AbstractRepository {
  def addPermission(value: Permission): Unit = ()
  def deletePermission(value: Permission): Unit = ()
}

 */
