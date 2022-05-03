package com.evolutiongaming.bootcamp.implicits

import akka.cluster.client.protobuf.msg.ClusterClientMessages.Contacts

import scala.annotation.tailrec

object ContextBounds extends App {

  object Exercise1 {

    // Let's revisit some functions we implemented while studying implicit classes:
    def concat(a: Int, b: Int): Int = (a.toString + b.toString).toInt

    // Exercise 1: Implement repeat functions for various types.
    // For `Int` using `concat` function above. For others, use built-in methods.

    // `repeat(72, 3)` should return `727272`
    def repeat(a: Int, times: Int): Int = {
      @tailrec
      def loop(result: Int, timesLeft: Int): Int =
        if (timesLeft == 1) result
        else loop(concat(a, result), timesLeft - 1)

      loop(a, times)
    }

    // `repeat("Scala", 3)` should return `"ScalaScalaScala"`
    def repeat(a: String, times: Int): String = a * times

    // `repeat(List(10, 20, 30), 3)` should return `List(10, 20, 30, 10, 20, 30, 10, 20, 30)`
    def repeat(a: List[Int], times: Int): List[Int] = ((a.mkString(" ") + " ") * times).split(" ").toList.map(_.toInt)

  }

  // Do you find anything common in these methods? These methods, basically, do
  // exactly the same job: calling concatenation method of the specific type.
  //
  // In some of programming languages, this is the most we can achieve. But not
  // in Scala. In Scala we have type parameters.

  // Exercise 2: Implement repeat functions so it works for `Int`, `String` and `List[Int]`.
  object Exercise2 {
    def repeat[T](a: T, times: Int): T = ???
    // impossible without implicits
  }

  // (spoiler was deleted here, will be told by lecturer)

  // What if we had a univeral way to concatenate all of these types? Let's make
  // such a univeral way...

  // Exercise 3:
  // - Implement `Concatenable` so it works for `Int`, `String` and `List[Int]`.
  // - Now implement `repeat` function using the new trait.

  trait ConcatenableEx3[T] {
    def concat(a: T, b: T): T
  }
  object ConcatenableEx3 {
    /*
    val forInt: ConcatenableEx3[Int] = new ConcatenableEx3[Int] {
      def concat(a: Int, b: Int): Int = (a.toString + b.toString).toInt
    }
    val forString: ConcatenableEx3[String] = new ConcatenableEx3[String] {
      def concat(a: String, b: String): String = a + b
    }
    val forListInt: ConcatenableEx3[List[Int]] = new ConcatenableEx3[List[Int]] {
      def concat(a: List[Int], b: List[Int]): List[Int] = a ++ b
    }*/
    val forInt: ConcatenableEx3[Int] = (a, b) => (a.toString + b.toString).toInt
    val forString: ConcatenableEx3[String] = _ + _
    val forListInt: ConcatenableEx3[List[Int]] = _ ++ _
  }

  object Exercise3 {
    def repeat[T](a: T, times: Int, concatenable: ConcatenableEx3[T]): T = {
      @tailrec
      def loop(result: T, timesLeft: Int): T =
        if (timesLeft <= 1) result
        else loop(concatenable.concat(a, result), timesLeft - 1)

      loop(a, times)
    }
  }

  // Do you find this way convenient / readable? What else we could do to
  // improve it? Make concatenable auto use by compiler

  // Exercise 4:
  // - Make the `repeat` method above more convenient to use by moving
  //   `Concatenable` to implicit parameter block.
  // - Make implicits for `Int`, `String` and `List[Int]` available by making
  //   the vals in `Concatenable` companion object implicit.

  trait Concatenable[T] {
    def concat(a: T, b: T): T
  }
  object Concatenable {
    implicit val forInt: Concatenable[Int] = (a, b) => (a.toString + b.toString).toInt
    implicit val forString: Concatenable[String] = _ + _
    implicit val forListInt: Concatenable[List[Int]] = _ ++ _
    /*
    implicit val forInt: Concatenable[Int] = new Concatenable[Int] {
      def concat(a: Int, b: Int): Int = (a.toString + b.toString).toInt
    }
    implicit val forString: Concatenable[String] = new Concatenable[String] {
      def concat(a: String, b: String): String = a + b
    }
    implicit val forListInt: Concatenable[List[Int]] = new Concatenable[List[Int]] {
      def concat(a: List[Int], b: List[Int]): List[Int] = a ++ b
    }*/
  }

  object Exercise4 {
    def repeat[T](a: T, times: Int)(implicit concatenable: Concatenable[T]): T = {
      @tailrec
      def loop(result: T, timesLeft: Int): T =
        if (timesLeft <= 1) result
        else loop(concatenable.concat(a, result), timesLeft - 1)

      loop(a, times)
    }
  }

  println("Exercise 4:")
  println(Exercise4.repeat(72, 3))
  println(Exercise4.repeat("Scala", 3))
  println(Exercise4.repeat(List(10, 20, 30), 3))

  // Now let's use our `repeat` method for something useful:
  // Exercise 5: implement the methods using `repeat` we just made.
  object Exercise5 {

    def repeatTenTimes[T](a: T)(implicit concatenable: Concatenable[T]): T = Exercise4.repeat(a, 10)
    def repeatTenTimesIfTrue[T](condition: Boolean)(a: T)(implicit concatenable: Concatenable[T]): Unit = if (condition) repeatTenTimes(a)

  }

  // While we made `repeatTenTimes` quite nice, but we still have to write
  // `implicit` keyword and it does not look really tidy because we have
  // `concatenable` parameter name despite not using it anywhere.
  //
  // The pattern is so prevalent in Scala that there is a special syntax sugar
  // to simplify the encoding called "context bounds".
  //
  // The following two methods are exactly the same:
  def method1[T](a: T)(implicit concatenable: Concatenable[T]): Unit = ()
  def method2[T: Concatenable](a: T): Unit = ()

  // Exercise 6: Use context bound to tidy up `repeat` method above
  object Exercise6 {
    // def repeat[T: ...](...): T = ???
    import Exercise4.repeat
    def repeatTenTimes[T: Concatenable](a: T): T = repeat(a, 10)
    def repeatTenTimesIfTrue[T: Concatenable](condition: Boolean)(a: T): Unit = if (condition) repeatTenTimes(a)
  }

  // Do you find this way convenient / readable? What else we could do to
  // improve it?
  //
  // We are still calling `repeat(argument, 7)` all the time instead of
  // `argument.repeat(7)`
  //
  // Turns out we can combine extension methods with implicit parameters
  // to create our own _syntax_.
  object Exercise7 {

    trait Concatenable[T] {
      def concat(a: T, b: T): T
    }

    object Concatenable {
      implicit val forInt: Concatenable[Int] = (a, b) => (a.toString + b.toString).toInt
      implicit val forString: Concatenable[String] = _ + _
      implicit val forListInt: Concatenable[List[Int]] = _ ++ _
      /*
      implicit val forInt: Concatenable[Int] = new Concatenable[Int] {
        def concat(a: Int, b: Int): Int = (a.toString + b.toString).toInt
      }
      implicit val forString: Concatenable[String] = new Concatenable[String] {
        def concat(a: String, b: String): String = a + b
      }
      implicit val forListInt: Concatenable[List[Int]] = new Concatenable[List[Int]] {
        def concat(a: List[Int], b: List[Int]): List[Int] = a ++ b
      }*/
    }

    implicit class ConcatSyntax[T: Concatenable](a: T) {
      def concat(b: T): T = implicitly[Concatenable[T]].concat(a, b)
    }

    implicit class RepeatSyntax[T: Concatenable](a: T) {
      def repeat(times: Int): T = {
        @tailrec
        def loop(result: T, timesLeft: Int): T =
          if (timesLeft <= 1) result
          else loop(a.concat(result), timesLeft - 1)

        loop(a, times)
      }
    }

    def repeatTenTimes[T: Concatenable](a: T): T = a.repeat(10)
    def repeatTenTimesIfTrue[T: Concatenable](condition: Boolean)(a: T): Unit = if (condition) repeatTenTimes(a)

    // 72.repeat(3)
    // "Scala".repeat(3)
    // List(10, 20, 30).repeat(3)

    // Exercise 7: Use the same approach to create syntax for `concat`
    // I.e. this should compile:
    // 72.concat(651)

  }
  import Exercise7.{ RepeatSyntax, ConcatSyntax }
  println("\nExercise 7:")
  println(72.repeat(3))
  println("Scala".repeat(3))
  println(List(10, 20, 30).repeat(3))

  println(72.concat(651))
}
