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

    def empty: A => B = PartialFunction.empty[A, B]
  }

  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))        === (a: String) => (a.length + a.toInt)
  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))("123") === 126

  println("2.5. Implement Monoid for Function1 (for result of the function)")
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
  trait Semigroupal[F[_]] extends Functor[F] {
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

    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]
  implicit class TupleOps[F[_]: Semigroupal, A, B](tuple1: (F[A], F[B])) {
    //def mapN[R](f: (A, B) => R): F[R] = (tuple1._1 product tuple1._2).map(f)
  }

  println("4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]")
  //println((Option(1), Option(2)).mapN(_ + _) == Some(3))
  //println((Option(1), None).mapN(_ + _)      == None)

  // 4.4. Implement Semigroupal for Map

  // (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  // 5.1. Implement Applicative for Option, Either

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???

  // traverse(List(1, 2, 3)) { i =>
  //   Option.when(i % 2 == 1)(i)
  // } == None

  // traverse(List(1, 2, 3)) { i =>
  //   Some(i + 1)
  // } == Some(List(2, 3, 4))
}
