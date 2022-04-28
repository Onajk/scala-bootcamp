package com.evolution.user.permission.domain

final case class PermissionName(value: String) extends AnyVal

final case class Permission(name: PermissionName)
