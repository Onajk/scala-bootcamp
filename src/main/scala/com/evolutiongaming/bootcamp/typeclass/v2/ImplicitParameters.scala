package com.evolutiongaming.bootcamp.typeclass.v2

import scala.util.Random

// Implicits: implicit parameters
//            implicit conversions

object ImplicitParameters extends App {

  //val name = "Oleg"
  implicit val name: String = "Oleg"

  //LuckService.greet(name)
  //LuckService.predictLuck(name)
  //LuckService.bye(name)
  LuckService.greet
  LuckService.predictLuck
  LuckService.bye
}

object LuckService {
  //def greet(name: String): Unit = println(s"Hello $name")
  //def predictLuck(name: String): Unit = println(s"Your luck is ${Random.nextInt(11)} today, $name")
  //def bye(name: String): Unit = println(s"See you $name")
  def greet(implicit name: String): Unit = println(s"Hello $name")
  def predictLuck(implicit name: String): Unit = println(s"Your luck is ${Random.nextInt(11)} today, $name")
  def bye(implicit name: String): Unit = println(s"See you $name")
}

object ImplicitParamTask {

  object Task1 {

    final case class User(id: String)

    trait DbConnection

    object DbConnection {
      def apply(): DbConnection = new DbConnection {}
    }

    // make second argument implicit
    //def createUser(user: User, connection: DbConnection): Unit = ???
    //createUser(User("123"), DbConnection())

    implicit val defaultConnection: DbConnection = DbConnection()

    def createUser(user: User)(implicit connection: DbConnection): Unit = ???
    createUser(User("123"))
  }

  object Task2 {
    //final case class Money(amount: Int)
    val list: List[Money] = ???
//    oh no, i won't compile
//    list.sorted

    final case class Money(amount: Int) extends Ordered[Money] {
      def compare(that: Money): Int = this.amount compare that.amount
    }

    list.sorted
  }
}
