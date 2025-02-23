package com.evolutiongaming.bootcamp.typeclass.v3_typeclass

final case class Json(s: String) { // simplified representation of JSON
  override def toString: String = s
}

object OOPJson extends App {

  trait Jsonable {
    def toJson: Json
  }

  def prettyPrint(jsonable: Jsonable): Unit = println(jsonable.toJson)

  final case class Game(id: Int) extends Jsonable {
    def toJson: Json = Json(s"{${'"'}id${'"'}:$id}")
  }

  prettyPrint(Game(123))
}

object FPJson extends App {

  trait Jsonable[A] {
    def toJson(entity: A): Json
  }

  def prettyPrint[A](a: A)(implicit jsonable: Jsonable[A]): Unit = println(jsonable.toJson(a))

  final case class Game(id: Int)

  implicit val gameJsonable: Jsonable[Game] = new Jsonable[Game] {
    def toJson(game: Game): Json = Json(s"{${'"'}id${'"'}:${game.id}}")
  }

  prettyPrint(Game(123))

  object InstancesTask {

    final case class Player(id: Int, name: String)

    implicit val playerJsonable: Jsonable[Player] = player => Json(s"{\"id\":${player.id},\"login\":\"${player.name}\"}")

    implicit val intJsonable: Jsonable[Int] = number => Json(s"{\"number\":$number}")

    implicit val optionIntJsonable: Jsonable[Option[Int]] = {
      case None => Json("null")
      case Some(value) => Json(s"{\"value\":$value}")
    }
  }

  object GenericImplicitsTask {

    implicit def optionJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[Option[A]] = {
      case Some(value) => jsonableA.toJson(value)
      case None => Json("null")
    }
    /*
    implicit def optionJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[Option[A]] =
      new Jsonable[Option[A]] {
        def toJson(entity: Option[A]): Json =
          entity match {
            case Some(value) => jsonableA.toJson(value)
            case None        => Json("null")
          }
      }
     */

    implicit def listJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[List[A]] =
      entity => Json(
        s"""
           |{
           |  array: [
           |    ${entity.map(x => jsonableA.toJson(x)).mkString(", ")}
           |    ]
           |}
           |""".stripMargin)
  }

  object SingleAbstractMethod {

    implicit val before: Jsonable[Game] = new Jsonable[Game] {
      def toJson(game: Game): Json = Json(s"{${'"'}id${'"'}:${game.id}}")
    }

    implicit val after: Jsonable[Game] = game => Json(s"{${'"'}id${'"'}:${game.id}}")
  }

  object ContextBound {

    def prettyPrintBefore[A](a: A)(implicit jsonable: Jsonable[A]): Unit = println(jsonable.toJson(a))

    def prettyPrintAfter[A: Jsonable](a: A): Unit = println(implicitly[Jsonable[A]].toJson(a))
  }

  object Summoner {

    object Jsonable {
      def apply[A](implicit instance: Jsonable[A]): Jsonable[A] = instance
    }

    def prettyPrintBefore[A: Jsonable](a: A): Unit = {
      val jsonable = implicitly[Jsonable[A]]
      println(jsonable.toJson(a))
    }

    def prettyPrintWithSummoner[A: Jsonable](a: A): Unit = println(Jsonable[A].toJson(a))
  }

  object Syntax {

    object Jsonable {
      def apply[A](implicit instance: Jsonable[A]): Jsonable[A] = instance
    }

    def prettyPrintBefore[A: Jsonable](a: A): Unit = println(Jsonable[A].toJson(a))

    object JsonableSyntax {
      implicit class JsonableOps[A](x: A) {
        def toJson(implicit jsonable: Jsonable[A]): Json = jsonable.toJson(x)
      }
    }

    import JsonableSyntax._
    def prettyPrintWithSyntax[A: Jsonable](a: A): Unit = a.toJson
  }
}

object FPJsonSugared extends App {

  trait Jsonable[T] {
    def toJson(entity: T): Json
  }

  object Jsonable {
    def apply[A: Jsonable]: Jsonable[A] = implicitly[Jsonable[A]]
  }

  object JsonableSyntax {
    implicit class JsonableOps[A](x: A) {
      def toJson(implicit jsonable: Jsonable[A]): Json = jsonable.toJson(x)
    }
  }

  import JsonableSyntax._
  def prettyPrint[A: Jsonable](a: A): Unit = println(a.toJson)

  final case class Game(id: Int)

  implicit val gameJsonable: Jsonable[Game] = (game: Game) => Json(s"{${'"'}id${'"'}:${game.id}}")

  prettyPrint(Game(123))
}

//object FPJsonMacros extends App {
//  import simulacrum._
//
//  @typeclass trait Jsonable[T] {
//    def toJson(entity: T): Json
//  }
//
//  import Jsonable.ops._
//  def prettyPrint[A: Jsonable](a: A): Unit = println(a.toJson)
//
//  final case class Game(id: Int)
//
//  implicit val gameJsonable: Jsonable[Game] = (game: Game) => Json(s"{${'"'}id${'"'}:${game.id}}")
//
//  prettyPrint(Game(123))
//}

object HashCodeTask {

  trait HashCode[A] {
    def hash(x: A): Int
  }

  object HashCode {
    def apply[A: HashCode]: HashCode[A] = implicitly[HashCode[A]]
  }

  implicit class HashCodeOps[A: HashCode](x: A) {
    def hash: Int = HashCode[A].hash(x)
  }

  // Implement an instance for String
  // Prove that I'm working
  implicit val hashForString: HashCode[String] = string => string.hashCode()

  def taskCheck[A: HashCode](x: A): Unit = println(x.hash)
  taskCheck("abc")
}