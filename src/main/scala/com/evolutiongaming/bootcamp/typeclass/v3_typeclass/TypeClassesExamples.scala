package com.evolutiongaming.bootcamp.typeclass.v3_typeclass

object TypeClassesExamples extends App {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A: Semigroup]: Semigroup[A] = implicitly[Semigroup[A]]
  }

  implicit class SemigroupSyntax[A: Semigroup](x: A) {
    def combine(y: A): A = Semigroup[A].combine(x, y)
  }

  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
  }

  // 2.2. Implement Monoid for Long, String
  def monoidHelper[A](zero: A, f: (A, A) => A): Monoid[A] = new Monoid[A] {
    def combine(x: A, y: A): A = f(x, y)
    def empty: A = zero
  }

  implicit val monoidForInt: Monoid[Int] = monoidHelper(0, _ + _)
  implicit val monoidForLong: Monoid[Long] = monoidHelper(0, _ + _)
  implicit val monoidForString: Monoid[String] = monoidHelper("", _ + _)

  // 1.2. Implement Semigroup for Long, String
  //implicit val semigroupForInt: Semigroup[Int] =  _ + _
  //implicit val semigroupForLong: Semigroup[Long] = _ + _
  //implicit val semigroupForString: Semigroup[String] = _ + _

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists
  def _combineAll[A: Semigroup](list: List[A]): A = list.reduceLeft(_ combine _)

  println("1.3. Implement combineAll(list: List[A]) for non-empty lists")
  println(_combineAll(List(1, 2, 3)) == 6)
  println(_combineAll(List("1", "2", "3")) == "123")

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists
  def combineAll[A: Semigroup](list: List[A], startingElement: A): A = list.foldLeft(startingElement)(_ combine _)

  println("1.4. Implement combineAll(list: List[A], startingElement: A) for all lists")
  println(combineAll(List(1, 2, 3), 0) == 6)
  println(combineAll(List(), 1) == 1)

  // 2.3. Implement combineAll(list: List[A]) for all lists
  def combineAll[A: Monoid](list: List[A]): A = list.foldLeft(Monoid[A].empty)(_ combine _)

  println("2.3. Implement combineAll(list: List[A]) for all lists")
  println(combineAll(List(1, 2, 3)) == 6)
  println(combineAll(List("1", "2", "3")) == "123")
  println(combineAll(List(1)) == 1)
  println(combineAll[Int](List()) == 0)

  // 2.4. Implement Monoid for Option[A]
  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(xs), Some(ys)) => Some(xs combine ys)
      case _ => x orElse y
    }
    def empty: Option[A] = None
  }

  println("2.4. Implement Monoid for Option[A]")
  println(combineAll(List(Some(1), None, Some(3))) == Some(4))
  println(combineAll[Option[Int]](List(None, None)) == None)
  println(combineAll[Option[Int]](List()) == None)
  // Can you make it work without specifying the type?
  //combineAll(List(None, None)) == None
  //combineAll(List()) == None

  // 2.5. Implement Monoid for Function1 (for result of the function)
  implicit def function1Monoid[A, B: Monoid]: Monoid[A => B] = new Monoid[A => B] {
    def combine(x: A => B, y: A => B): A => B = (a: A) => x(a) combine y(a)

    //def empty: A => B = PartialFunction.empty[A, B]
    def empty: A => B = _ => Monoid[B].empty
  }

  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))        === (a: String) => (a.length + a.toInt)
  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))("123") === 126

  println("2.5. Implement Monoid for Function1 (for result of the function)")
  println((((a: String) => a.length) combine ((a: String) => a.toInt))("123") == 126)
  println((Monoid[String => Int].empty combine ((a: String) => a.toInt))("123") == 123)
  println((((a: String) => a.toInt) combine Monoid[String => Int].empty)("123") == 123)
  //println(combineAll(List((a: String) => a.length, (a: String) => a.toInt))("123"))

  // 3. Functor
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def map[B](f: A => B): F[B] = Functor[F].map(fa)(f)
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product` method,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_]: Semigroupal]: Semigroupal[F] = implicitly[Semigroupal[F]]
  }

  implicit class SemigroupalOps[F[_]: Semigroupal, A](fa: F[A]) {
    def product[B](fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
  }

  // 4.2. Implement Semigroupal for Option
  implicit val optionSemigroupal: Semigroupal[Option] = new Semigroupal[Option] {
    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

  /*
  println("4.2. Implement Semigroupal for Option")
  println((Option(1) product Option(2)) == Option(1, 2))
  println((Option(1) product Option.empty[Int]) == None)
  */

  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]

  trait Apply[F[_]] extends Semigroupal[F] with Functor[F] {
    def ap[A, B](fab: F[A => B], fa: F[A]): F[B]

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val helperFunction: A => B => (A, B) = (a: A) => (b: B) => (a, b)
      val fab: F[B => (A, B)] = map(fa)(helperFunction)
      ap(fab, fb)
    }

    def mapN[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      map(product(fa, fb)) {
        case (a, b) => f(a, b)
      }
    }
  }

  object Apply {
    def apply[F[_]: Apply]: Apply[F] = implicitly[Apply[F]]
  }

  implicit class ApplyOps[F[_]: Apply, A, B](fafb: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = Apply[F].mapN(fafb._1, fafb._2)(f)
  }

  implicit val optionApply: Apply[Option] = new Apply[Option] {
    def ap[A, B](fab: Option[A => B], fa: Option[A]): Option[B] = for {
      f <- fab
      a <- fa
    } yield f(a)

    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  /*
  println("4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]")
  println((Option(1), Option(2)).mapN(_ + _) == Some(3))
  println((Option(1), Option.empty[Int]).mapN(_ + _)      == None)
  */

  // 4.4. Implement Semigroupal for Map

  implicit def mapApply[T]: Apply[Map[T, *]] = new Apply[Map[T, *]] {
    def ap[A, B](fab: Map[T, A => B], fa: Map[T, A]): Map[T, B] = {
      val tuple = for (a <- fa) yield (a, fab.get(a._1))
      for {
        ((k, v), optionF) <- tuple
        f <- optionF
      } yield (k, f(v))
    }

    def map[A, B](fa: Map[T, A])(f: A => B): Map[T, B] = fa.transform((_, value) => f(value))
  }

  println("4.4. Implement Semigroupal for Map")
  println((Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc"))

  // 5. Applicative
  //trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
  trait Applicative[F[_]] extends Apply[F] {
    def pure[A](x: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f), fa)
  }

  // 5.1. Implement Applicative for Option, Either

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def pure[A](x: A): Option[A] = Some(x)

    def ap[A, B](fab: Option[A => B], fa: Option[A]): Option[B] = for {
      f <- fab
      a <- fa
    } yield f(a)
  }

  implicit def eitherApplicative[L]: Applicative[Either[L, *]] = new Applicative[Either[L, *]] {
    def pure[A](x: A): Either[L, A] = Right(x)

    def ap[A, B](fab: Either[L, A => B], fa: Either[L, A]): Either[L, B] = for {
      f <- fab
      a <- fa
    } yield f(a)
  }

  println("4.2. Implement Semigroupal for Option")
  println((Option(1) product Option(2)) == Option(1, 2))
  println((Option(1) product Option.empty[Int]) == None)

  println("4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]")
  println((Option(1), Option(2)).mapN(_ + _) == Some(3))
  println((Option(1), Option.empty[Int]).mapN(_ + _)      == None)

  println("5.1. Implement Applicative for Option, Either")
  val test1: Either[String, Int] = Right(5)
  val test2: Either[String, Int] = Right(4)
  val test3: Either[String, Int] = Left("error")
  println((test1, test2).mapN(_ + _) == Right(9))
  println((test1, test3).mapN(_ + _) == Left("error"))

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List.empty[B]): Option[List[B]]) { (a: A, acc: Option[List[B]]) =>
      val optB: Option[B] = f(a)
      (optB, acc).mapN(_ :: _)
    }

  println("5.2. Implement `traverse` for all Applicatives instead of Option")
  println(
    traverse(List(1, 2, 3)) { i =>
      Option.when(i % 2 == 1)(i)
    } == None)

  println(
    traverse(List(1, 2, 3)) { i =>
      Some(i + 1)
    } == Some(List(2, 3, 4)))
}
