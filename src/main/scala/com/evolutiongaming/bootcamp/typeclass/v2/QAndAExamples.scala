package com.evolutiongaming.bootcamp.typeclass.v2

import cats.Semigroupal
import com.evolutiongaming.bootcamp.typeclass.v2.QAndAExamples.EmptyGroup

object QAndAExamples extends App {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A](implicit instance: Semigroup[A]): Semigroup[A] = instance
  }

  implicit class SemigroupSyntax[A](x: A) {
    def combine(y: A)(implicit group: Semigroup[A]): A = group.combine(x, y)
  }

  // 1.2. Implement Semigroup for Long, String
  implicit val semigroupForInt: Semigroup[Int] = (first, second) => first + second
  implicit val semigroupForLong: Semigroup[Long] = (first, second) => first + second
  implicit val semigroupForString: Semigroup[String] = (first, second) => (first.toInt + second.toInt).toString

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists
  def _combineAll[A: Semigroup](list: List[A]): A = list.reduceLeft((first, second) => first.combine(second))

  println(_combineAll(List(1, 2, 3)) == 6)
  println(_combineAll(List("1", "2", "3")) == "6")

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists
  def combineAll[A: Semigroup](list: List[A], startingElement: A): A = list.foldLeft(startingElement)((first, second) => first.combine(second))

  println(combineAll(List(1, 2, 3), 0) == 6)
  println(combineAll(List(), 1) == 1)

  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup
  trait EmptyGroup[A] extends Semigroup[A] {
    def empty: A
  }

  object EmptyGroup {
    def apply[A](implicit instance: EmptyGroup[A]): EmptyGroup[A] = instance
  }

  // 2.2. Implement Monoid for Long, String
  implicit val groupForInt: EmptyGroup[Int] = new EmptyGroup[Int] {
    def combine(x: Int, y: Int): Int = x + y
    def empty: Int = 0
  }

  implicit val groupForString: EmptyGroup[String] = new EmptyGroup[String] {
    def combine(x: String, y: String): String = (x.toInt + y.toInt).toString
    def empty: String = ""
  }

  // 2.3. Implement combineAll(list: List[A]) for all lists
  //def combineAll[A: EmptyGroup](list: List[A]): A = list.foldLeft(EmptyGroup[A].empty)((first, second) => first.combine(second))

  //println(combineAll(List(1, 2, 3)) == 6)
  //println(combineAll(List("1", "2", "3")) == "6")

  // 2.4. Implement Monoid for Option[A]

  // combineAll(List(Some(1), None, Some(3))) == Some(4)
  // combineAll(List(None, None)) == None
  // combineAll(List()) == None

  // 2.5. Implement Monoid for Function1 (for result of the function)

  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))        === (a: String) => (a.length + a.toInt)
  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))("123") === 126

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)

  // 4.2. Implement Semigroupal for Option

  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]

  // (Option(1), Option(2)).mapN(_ + _) == Some(3)
  // (Option(1), None).mapN(_ + _)      == None

  // 4.4. Implement Semigroupal for Map

  // (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  // 5.1. Implement Applicative for Option, Either
  new Applicative[Either[String, *]] {
    override def pure[A](x: A): Either[String, A] = Right(x)
    override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] = ???
    override def fmap[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = ???
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???

  // traverse(List(1, 2, 3)) { i =>
  //   Option.when(i % 2 == 1)(i)
  // } == None

  // traverse(List(1, 2, 3)) { i =>
  //   Some(i + 1)
  // } == Some(List(2, 3, 4))

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library

  // 6.3. Implement `traverse` for all Foldables instead of List
}
