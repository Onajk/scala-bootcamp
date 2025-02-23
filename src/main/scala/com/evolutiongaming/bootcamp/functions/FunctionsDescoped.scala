package com.evolutiongaming.bootcamp.functions

object FunctionsDescoped {
  //
  var count = 0
  def id(): Int = {
    val newId = count
    count += 1
    newId
  }
  def idPure(count: Int): (Int, Int) = (count + 1, count)

  // Implement `identity` which returns its input unchanged. Do not use scala.Predef.identity
  def identity[A](x: A): A = x

  // Question. What do you expect?

  val f1: PartialFunction[List[String], Boolean] = {
    // head :: tail
    case _ :: _ => true
  }

  // 1
  val result1: Boolean = f1.isDefinedAt(List("false", "true"))

  val f2: PartialFunction[List[String], Boolean] = {
    case Nil => false
    // head :: 2nd :: tail
    case _ :: _ :: tail => f1(tail)
  }

  // 2
  val result2: Boolean = f2.isDefinedAt(List("false", "true"))

  // --

  // Functions can be used as building blocks of our program using the composition of functions
  // `scala.Function1[A, B]` has `compose` and `andThen` methods that takes a function param and returns a new function

  // Compose - `g` will be applied to input param
  // def compose[A](g: A => T1): A => R = { x => apply(g(x)) }

  val double: Int => Int = (x: Int) => 2 * x
  val addString: Int => String = (a: Int) => "new value " + a

  addString.compose(double)

  // AndThen - `g` will be applied to output result
  // def andThen[A](g: R => A): T1 => A = { x => g(apply(x)) }

  double.andThen(addString)

  List(1, 2, 3).map(_ + 2).map(_.toString)
  List(1, 2, 3).map(((x: Int) => x + 2).andThen(x => x.toString))

  // Exercise. Implement `andThen` and `compose` which pipes the result of one function to the input of another function
  def compose[A, B, C](f: B => C, g: A => B): A => C = { x => f(g(x)) }

  def andThen[A, B, C](f: A => B, g: B => C): A => C = { x => g(f(x)) }


  // --


  // Final task.
  // Case classes are Scala's preferred way to define complex data

  val rawJson: String =
    """
      |{
      |   "username":"John",
      |   "address":{
      |      "country":"UK",
      |      "postalCode":45765
      |   },
      |   "eBooks":[
      |      "Scala",
      |      "Dotty"
      |   ]
      |}
  """.stripMargin

  // Representing JSON in Scala as a sealed family of case classes
  // JSON is a recursive data structure
  sealed trait Json

  case class JObject(fields: Map[String, Json]) extends Json

  case class JArray(items: List[Json]) extends Json

  case class JString(value: String) extends Json

  case class JNumber(value: BigDecimal) extends Json

  case class JBoolean(value: Boolean) extends Json

  // Question. What did I miss? null type
  case object Null extends Json
  // --

  // Task 1. Represent `rawJson` string via defined classes
  val data: Json = JObject(Map(
    "username" -> JString("John"),
    "address" -> JObject(Map(
      "country" -> JString("UK"),
      "postalCode" -> JNumber(45765))),
    "eBooks" -> JArray(List(
      JString("Scala"),
      JString("Dotty")))
    ))

  // Task 2. Implement a function `asString` to print given Json data as a json string

  def asString(data: Json): String = data match {
    case Null => ""
    case JBoolean(value) => s"$value"
    case JNumber(value) => s"$value"
    case JString(value) => s"\"$value\""
    case JArray(items) => "[" + items.map(x => asString(x)).mkString(", ") + "]"
    case JObject(fields) => "{" + fields.toList.map(x => s"\"${x._1}\":${asString(x._2)}").mkString(", ") + "}"
  }

  // Task 3. Implement a function that validate our data whether it contains JNumber with negative value or not

  def isContainsNegative(data: Json): Boolean =
    data match {
      case JNumber(value) if value < 0 => true
      case JArray(items) => items.exists(x => isContainsNegative(x))
      case JObject(fields) => fields.exists(x => isContainsNegative(x._2))
      case _ => false
    }

  // Task 4. Implement a function that return the nesting level of json objects.
  // Note. top level json has level 1, we can go from top level to bottom only via objects

  def nestingLevel(data: Json): Int =
    data match {
      case JObject(fields) => fields.map(x => 1 + nestingLevel(x._2)).max
      case _ => 0
    }

  // See FunctionsSpec for expected results

  // If expected type is a PF then a pattern matching block will expended to PF implementation

  val pingPongPFImpl: PartialFunction[String, String] = new PartialFunction[String, String] {
    override def isDefinedAt(x: String): Boolean = x match {
      case "ping" => true
      case _ => false
    }

    override def apply(v: String): String = v match {
      case "ping" => "pong"
    }
  }

  // Example of using partial functions:
  val eithers: Seq[Either[String, Double]] = List("123", "456", "789o")
    .map(x => x.toDoubleOption.toRight(s"Failed to parse $x"))

  val errors: Seq[String] = eithers.collect {
    case Left(x) => x
  }
}
