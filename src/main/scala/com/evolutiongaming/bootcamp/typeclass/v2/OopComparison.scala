package com.evolutiongaming.bootcamp.typeclass.v2

import com.evolutiongaming.bootcamp.typeclass.v2.Fp.{Jsonable, User}
import com.evolutiongaming.bootcamp.typeclass.v2.Summoner.Jsonable

final case class Json(s: String) // my very basic json class

object Oop extends App {
  trait Jsonable {
    def toJson: Json
  }

  // my imaginary method which makes use of Jsonable
  def printBeautifully(x: Jsonable): Unit = {
    println(x.toJson)
  }

  // my entity
  final case class User(name: String) extends Jsonable {
    def toJson: Json = Json(s"{name: $name}") // Jsonable implementation for User
  }

  printBeautifully(User("Oleg"))
}

// the main idea behind typeclass is here
// ask questions if you can't understand what is going on
object Fp extends App {

  trait Jsonable[T] { // <- the typeclass itself: a trait with one type parameter
    //           ^
    //   the type parameter
    def toJson(entity: T): Json // it may have many methods but usually one
  }

  // a typeclass describes an interface which can be implemented for different types

  // my imaginary method changed its signature a bit
  def printBeautifully[A](x: A)(implicit jsonable: Jsonable[A]): Unit = {
    println(jsonable.toJson(x))
  }

  // my entity does not implement anything and can be implemented with no idea Jsonable exists
  final case class User(name: String)

  // here goes the implementation
  // we can define it not touching User or Jsonable or anything
  implicit val userJsonable: Jsonable[User] = user => Json(s"{name: ${user.name}}")

  //implicit val userJsonable: Jsonable[User] = new Jsonable[User] {
  //  def toJson(user: User): Json = Json(s"{name: ${user.name}}")
  //}

  // the usage is the same
  printBeautifully(User("Oleg"))

  object InstancesTask {
    final case class Player(id: Int, login: String)

    implicit val playerJsonable: Jsonable[Player] = player => Json(s"{id: ${player.id}, login: ${player.login}}")

    implicit val intJsonable: Jsonable[Int] = number => Json(s"{number: ${number}}")

    implicit val optionIntJsonable: Jsonable[Option[Int]] = {
      case None => Json("null")
      case Some(value) => Json(s"{value: ${value}}")
    }

    //implicit val playerJsonable: Jsonable[Player] = new Jsonable[Player] {
    //  def toJson(player: Player): Json = Json(s"{id: ${player.id}, login: ${player.login}}")
    //}

    //implicit val intJsonable: Jsonable[Int] = new Jsonable[Int] {
    //  def toJson(number: Int): Json = Json(s"{number: ${number}}")
    //}

    //implicit val optionIntJsonable: Jsonable[Option[Int]] = new Jsonable[Option[Int]] {
    //  def toJson(option: Option[Int]): Json = option match {
    //    case None => Json("null")
    //    case Some(value) => Json(s"{value: ${value}}")
    //  }
    //}
  }

  // you will definitely get it but maybe a bit later and its ok
  object GenericImplicitsTask {
    // the thing can convert to json any options which is super useful
    implicit def optionJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[Option[A]] = {
      case Some(value) => jsonableA.toJson(value)
      case None => Json("null")
    }

    //implicit def optionJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[Option[A]] = new Jsonable[Option[A]] {
    //  def toJson(entity: Option[A]): Json = {
    //    entity match {
    //      case Some(value) => jsonableA.toJson(value)
    //      case None => Json("null")
    //    }
    //  }
    //}

    implicit def listJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[List[A]] =
      entity => Json(s"{array: ${entity.map(x => jsonableA.toJson(x)).mkString(", ")}}")

    //implicit def listJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[List[A]] = new Jsonable[List[A]] {
    //  def toJson(entity: List[A]): Json = Json(s"{array: ${entity.map(x => jsonableA.toJson(x)).mkString(", ")}}")
    //}
  }
  import InstancesTask._
  import GenericImplicitsTask._

  printBeautifully(Player(1, "login1"))
  printBeautifully(5)
  printBeautifully[Option[Int]](None)
  printBeautifully[Option[Int]](Some(5))
  printBeautifully[Option[Player]](Some(Player(2, "login2")))
  printBeautifully(List(1, 2, 3))
  printBeautifully(List(Player(1, "login1"), Player(2, "login2"), Player(3, "login3")))
}

// lets add pieces of sugar one by one
// don't try to remember everything

object SingleAbstractMethod {
  // they are the same, choose any style you like

  implicit val was: Jsonable[User] = new Jsonable[User] {
    def toJson(user: User): Json = Json(s"{name: ${user.name}")
  }

  implicit val now: Jsonable[User] = user => Json(s"{name: ${user.name}")

  // TODO: go back to your InstancesTask and change your instances to SAM
}

object ContextBound {
  def printBeautifullyOld[A](x: A)(implicit jsonable: Jsonable[A]): Unit = {
    println(jsonable.toJson(x))
  }

  def printBeautifully[A: Jsonable](x: A): Unit = {
    val jsonable = implicitly[Jsonable[A]]
    println(jsonable.toJson(x))
  }
}

object Summoner {
  object Jsonable { // but it gets a companion object

    // with nice summon method (could have any name, apply for eg)
    def apply[F](implicit instance: Jsonable[F]): Jsonable[F] = instance
  }

  // so now we can change
  def printBeautifullyOld[A: Jsonable](x: A): Unit = {
    val jsonable = implicitly[Jsonable[A]]
    println(jsonable.toJson(x))
  }

  // to
  def printBeautifully[A: Jsonable](x: A): Unit = {
    println(Jsonable[A].toJson(x))
  }
}

object Syntax {

  object JsonableSyntax {

    implicit class JsonableOps[A](x: A) {
      def toJson(implicit j: Jsonable[A]): Json = {
        j.toJson(x)
      }
    }

  }

  // so now we can change
  def printBeautifullyOld[A: Jsonable](x: A): Unit = {
    println(Jsonable[A].toJson(x))
  }

  // to
  import JsonableSyntax._
  def printBeautifully[A: Jsonable](x: A): Unit = {
    println(x.toJson)
  }
}


object Result {

  // --- json library (provides the typeclass) ---
  trait Jsonable[T] {
    def toJson(entity: T): Json
  }

  object Jsonable {
    def apply[F](implicit instance: Jsonable[F]): Jsonable[F] = instance
  }

  object JsonableSyntax {

    implicit class JsonableOps[A](x: A) {
      def toJson(implicit j: Jsonable[A]): Json = {
        j.toJson(x)
      }
    }
  }

  // --- library which makes use of json (for example some http library) ---
  import JsonableSyntax._
  def printBeautifully[A: Jsonable](x: A): Unit = {
    println(x.toJson)
  }

  // --- domain library of your project ---
  final case class User(name: String)

  // --- domain utils library ---
  implicit val JsonableUser: Jsonable[User] = user => Json(s"{name: ${user.name}") // good luck choosing name

  // --- you ---
  printBeautifully(User("Paweł"))
}

// having two implementations for the same type (like different ways to make json out of User) is possible
// but considered to be bad

object TypeclassTask extends App {

  // Why am I not a Typeclass?
  // TODO: Rework me so I am a typeclass
  trait HashCode[A] {
    def hash(x: A): Int
  }

  object HashCode {
    // TODO: Implement me a summoner
    def apply[A: HashCode]: HashCode[A] = implicitly[HashCode[A]]
  }

  implicit class HashCodeSyntax[A: HashCode](x: A) {
    // TODO: Implement syntax so I can "abc".hash
    //def hash(implicit obj: HashCode[A]): Int = obj.hash(x)
    // because of apply[A] I can change above to below
    def hash: Int = HashCode[A].hash(x)
  }

  // TODO: make an instance for String
  implicit val hashForString: HashCode[String] = string => string.map(_.toInt).sum
  //implicit val hashForInt: HashCode[Int] = value => value
  implicit val hashForInt: HashCode[Int] = identity
  // TODO: write "abc".hash to check everything
  def taskCheck[A: HashCode](x: A): Unit = println(x.hash)
  taskCheck("abc")
  taskCheck(5)
}
