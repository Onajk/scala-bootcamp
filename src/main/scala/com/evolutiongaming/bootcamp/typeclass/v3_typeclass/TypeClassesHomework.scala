package com.evolutiongaming.bootcamp.typeclass.v3_typeclass

/**
  * Try to accomplish as many tasks as you can
  */
object TypeClassesHomework {

  object OrderingTask {

    final case class Money(amount: BigDecimal)

    // TODO Implement Ordering instance for Money
    implicit val moneyOrdering: Ordering[Money] = (x, y) => x.amount compare y.amount
  }

  object ShowTask {

    trait Show[T] { // Fancy toString
      def show(entity: T): String
    }

    final case class User(id: String, name: String)

    // TODO Implement Show instance for User
    implicit val userShow: Show[User] = user => s"User(id:${user.id}, name:${user.name})"

    // TODO Implement syntax for Show so I can do User("1", "John").show
    implicit class ShowSyntax[T: Show](x: T) {
      def show: String = implicitly[Show[T]].show(x)
    }
  }

  object ParseTask {

    type Error = String

    trait Parse[T] { // Feel free to use any format. It could be CSV or anything else.
      def parse(entity: String): Either[Error, T]
    }

    final case class User(id: String, name: String)

    // TODO Implement Parse instance for User
    implicit val parseUser: Parse[User] = _.split(",").toList match {
      case id :: name :: Nil => Right(User(id.trim, name.trim))
      case _ => Left("It is not an User")
    }

    // TODO Implement syntax for Parse so I can do "lalala".parse[User] (and get an error because it is obviously not a User)
    implicit class parseSyntax(x: String) {
      def parse[T: Parse]: Either[Error, T] = implicitly[Parse[T]].parse(x)
    }

    println("lalala".parse[User])
  }

  object EqualsTask {
    // TODO Design a typesafe equals so I can do a === b, but it won't compile if a and b are of different types
    // Define the typeclass (think of a method signature)
    // Keep in mind that `a method b` is `a.method(b)`

    implicit class TypesafeSyntax[A](a: A) {
      def ===(b: A): Boolean = a == b
    }
    //println(5 === 5L) // won't compile
    println(5 === 5)
  }

  object Foldable {

    trait Semigroup[A] {
      def combine(x: A, y: A): A
    }

    trait Monoid[A] extends Semigroup[A] {
      def empty: A
    }

    trait Foldable[F[_]] {
      def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
      def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
      def foldMap[A, B](as: F[A])(f: A => B)(implicit monoid: Monoid[B]): B
    }

    // TODO Implement Foldable instance for Option
    implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
      def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      def foldMap[A, B](as: Option[A])(f: A => B)(implicit monoid: Monoid[B]): B = as.map(f).getOrElse(monoid.empty)
    }

    // TODO Implement Foldable instance for List
    implicit val listFoldable: Foldable[List] = new Foldable[List] {
      def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      def foldMap[A, B](as: List[A])(f: A => B)(implicit monoid: Monoid[B]): B = foldLeft(as)(monoid.empty)((acc, a) => monoid.combine(acc, f(a)))
    }

    sealed trait Tree[A]
    object Tree {
      final case class Leaf[A](value: A) extends Tree[A]
      final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    }

    // TODO Implement Foldable instance for Tree
    implicit val treeFoldable: Foldable[Tree] = new Foldable[Tree] {
      def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
        case Tree.Leaf(value) => f(z, value)
        case Tree.Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
      }

      def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
        case Tree.Leaf(value) => f(value, z)
        case Tree.Branch(left, right) => foldRight(right)(foldRight(left)(z)(f))(f)
      }

      def foldMap[A, B](as: Tree[A])(f: A => B)(implicit monoid: Monoid[B]): B =
        foldLeft(as)(monoid.empty)((acc, a) => monoid.combine(acc, f(a)))
    }
  }

  object ApplicativeTask {

    trait Semigroupal[F[_]] {
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
    }

    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }

    trait Apply[F[_]] extends Functor[F] with Semigroupal[F] {

      def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] // "ap" here stands for "apply" but it's better to avoid using it

      // TODO Implement using `ap` and `map`
      override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
        val helperFunction: A => B => (A, B) = (a: A) => (b: B) => (a, b)
        val fab: F[B => (A, B)] = map(fa)(helperFunction)
        ap(fab)(fb)
      }

      // TODO Implement using `map` and `product`
      def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = {
        map(product(fa, fb)) {
          case (a, b) => f(a, b)
        }
      }
    }

    trait Applicative[F[_]] extends Apply[F] {
      def pure[A](a: A): F[A]

      def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
    }

    // TODO Implement Applicative instantce for Option
    implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
      def pure[A](a: A): Option[A] = Some(a)

      def ap[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = for {
        f <- fab
        a <- fa
      } yield f(a)
    }

    // TODO Implement traverse using `map2`
    def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(implicitly[Applicative[F]].pure(List.empty[B]): F[List[B]]) { (a: A, acc: F[List[B]]) =>
        val fB: F[B] = f(a)
        implicitly[Applicative[F]].map2(fB, acc)(_ :: _)
      }

    // TODO Implement sequence (ideally using already defined things)
    def sequence[F[_]: Applicative, A](fas: List[F[A]]): F[List[A]] =
      //traverse(fas)(fa => fa)
      traverse(fas)(identity)
  }
}
