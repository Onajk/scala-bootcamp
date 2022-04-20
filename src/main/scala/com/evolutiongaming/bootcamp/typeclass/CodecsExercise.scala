package com.evolutiongaming.bootcamp.typeclass

object CodecsExercise extends App {
  sealed trait Json
  final case object JsonNull extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json
  final case class JsonArray(value: List[Json]) extends Json
  final case class JsonObject(value: Map[String, Json]) extends Json

  // Encoder
  trait Encoder[A] {
    def toJson(a: A): Json
  }

  object Encoder {
    def apply[A: Encoder]: Encoder[A] = implicitly[Encoder[A]]
  }

  implicit class EncoderOps[A: Encoder](a: A) {
    def toJson: Json = Encoder[A].toJson(a)
  }

  // Decoder
  trait Decoder[A] {
    def fromJson(json: Json): Option[A]
  }

  object Decoder {
    def apply[A: Decoder]: Decoder[A] = implicitly[Decoder[A]]
  }

  implicit class DecoderOps(json: Json) {
    def as[A: Decoder]: Option[A] = Decoder[A].fromJson(json)
  }

  // Exercise 1. Implement Encoder and Decoder for Int.
  implicit val IntEncoder: Encoder[Int] = JsonInt(_)
  implicit val IntDecoder: Decoder[Int] = {
    case JsonInt(value) => Some(value)
    case _ => None
  }

  println("Exercise 1. Implement Encoder and Decoder for Int.")
  println(100.toJson)
  println(JsonNull.as[Int])

  // Exercise 2. Implement Encoder and Decoder for String.
  implicit val StringEncoder: Encoder[String] = JsonString(_)
  implicit val StringDecoder: Decoder[String] = {
    case JsonString(value) => Some(value)
    case _ => None
  }

  println("\nExercise 2. Implement Encoder and Decoder for String.")
  println("Example".toJson)
  println(JsonNull.as[String])


  final case class Person(name: String, age: Int)

  // Exercise 3. Implement Encoder and Decoder for Person.
  implicit val PersonEncoder: Encoder[Person] = person => JsonObject(Map("name:" -> person.name.toJson, "age:" -> person.age.toJson))
  implicit val PersonDecoder: Decoder[Person] = {
    case JsonObject(value) => for {
      nameJson <- value.get("name:")
      ageJson <- value.get("age:")
      name <- nameJson.as[String]
      age <- ageJson.as[Int]
    } yield Person(name, age)
    case _ => None
  }

  println("\nExercise 3. Implement Encoder and Decoder for Person.")
  println(Person("Ivan", 25).toJson)
  println(JsonNull.as[Person])
  println(JsonObject(Map("name:" -> JsonString("Ivan"), "age:" -> JsonInt(25))).as[Person])

  // Exercise 4. Implement Encoder and Decoder for List with any content.
  implicit def listEncoder[A: Encoder]: Encoder[List[A]] = list => JsonArray(list.map(_.toJson))
  implicit def listDecoder[A: Decoder]: Decoder[List[A]] = {
    case JsonArray(list) => Some(for {
      json <- list
      fromJson <- json.as[A]
    } yield fromJson)
    case _ => None
  }

  println("Exercise 4. Implement Encoder and Decoder for List with any content.")
  println(List(1, 2, 3, 4, 5).toJson)
  println(JsonNull.as[List[Int]])
  println(JsonArray(List(JsonInt(1), JsonInt(2), JsonInt(3), JsonInt(4), JsonInt(5))).as[List[Int]])

  final case class EntityId(id: String) extends AnyVal

  // Exercise 5. Implement Encoder and Decoder for EntityId with any content.
  implicit val idEncoder: Encoder[EntityId] = ???
  implicit val idDecoder: Decoder[EntityId] = ???


  // Exercise 6. Describe Functor
  // 1. Typeclass itself: `trait Functor`
  // 2. Typeclass Summoner: `object Functor`
  // 3. Typeclass Ops: `implicit class FunctorOps`

  // Exercise 7. Implement Functor for decoder: `implicit val decoderFunctor`

  // Exercise 8. Describe Contravariant
  // 1. Typeclass itself: `trait Contravariant`
  // 2. Typeclass Summoner: `object Contravariant`
  // 3. Typeclass Ops: `implicit class ContravariantOps`

  // Exercise 9. Implement Contravariant for encoder: `implicit val encoderContravariant`

  // Functions Example
  val foo1: String => Int = _.length
  val foo2: Boolean => String = if (_) "100" else "1"

  // Exercise 10. Implement Functor and Contravariant for functions:
  // implicit def functionFunctor
  // implicit def functionContravariant

  // val foo3: Boolean => Int = functionFunctor.fmap(foo2)(foo1)
  // val foo4: Boolean => Int = functionContravariant.contramap(foo1)(foo2)
}
