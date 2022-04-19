package com.evolutiongaming.bootcamp.typeclass.v2

// make as many exercises as you can

object Task1 {
  final case class Money(amount: BigDecimal)

  // TODO: create Ordering instance for Money
  implicit val moneyOrdering: Ordering[Money] = (x, y) => x.amount compare y.amount
}

object Task2 extends App {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  // TODO: create Show instance for User
  implicit val userShow: Show[User] = user => s"id: ${user.id}, name: ${user.name}"

  // TODO: create syntax for Show so i can do User("1", "Oleg").show
  implicit class ShowSyntax[T](x: T) {
    def show(implicit instance: Show[T]): String = instance.show(x)
  }

  println(User("1", "Oleg").show)
}

object Task3 extends App {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  final case class Table(id: Int, value: String)

  // TODO: create Parse instance for User
  implicit val parseUser: Parse[User] = entity => entity.split(",").toList match {
    case id :: name :: Nil => Right(User(id.trim, name.trim))
    case _ => Left("It is not an User")
  }

  implicit val parseTable: Parse[Table] = entity => entity.split(",").toList match {
    case id :: value :: Nil => Right(Table(id.trim.toInt, value.trim))
    case _ => Left("It is not a Table")
  }

  // TODO: create syntax for Parse so i can do "lalala".parse[User] (and get an error because it is obviously not a User)
  implicit class parseSyntax(x: String) {
    def parse[T](implicit instance: Parse[T]): Either[Error, T] = instance.parse(x)
  }

  println("lalala".parse[User])
  println("1, Paweł".parse[User])
  println("1, Paweł".parse[Table])
}

object Task4 extends App {
  // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`

  implicit class TypesafeSyntax[A](a: A) {
    def ===(b: A): Boolean = a == b
  }

  //println(5 === 5L) won't compile
  println(5 === 5)
}

object AdvancedHomework {
  // TODO: create a typeclass for flatMap method
}
