package com.evolution.casino.updater

object Utils {
  def log(value: String): Unit = println(value)
  def logDebug(value: String): Unit = println(value)
  def validate(value: String): Option[String] = Some(value)
}
