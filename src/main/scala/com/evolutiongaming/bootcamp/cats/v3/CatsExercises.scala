package com.evolutiongaming.bootcamp.cats.v3

import cats.syntax.all._
import cats.{Applicative, Monad}

object CatsExercises {
  trait AltApplicative[F[_]] {
    def pure[A](a: A): F[A]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  }

  // Implement AltApplicative with cats.Applicative.
  def catsToAltApplicative[F[_]](implicit F: Applicative[F]): AltApplicative[F] = {
    new AltApplicative[F] {
      override def pure[A](a: A): F[A] = a.pure[F]

      // Implement map2 here, don't use F.map2
      // Hint: f.curried will help, multiple uses of ap may be needed
      override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = F.ap(F.ap(pure(f.curried))(fa))(fb)
    }
  }

  // Implement applicative composition via map2
  // I.e. for applicatives F and G, implement applicative for F[G[_]]
  def composedAltApplicative[F[_], G[_]](implicit F: AltApplicative[F], G: AltApplicative[G]): AltApplicative[Lambda[T => F[G[T]]]] = {
    new AltApplicative[Lambda[T => F[G[T]]]] {
      override def pure[A](x: A): F[G[A]] = F.pure(G.pure(x))

      // Implement map2 here
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] = F.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

  // Implement Functor's map with Applicative (ap, pure)
  def mapFromApplicative[F[_]: Applicative, A, B](fa: F[A])(f: A => B): F[B] = Applicative[F].ap(Applicative[F].pure(f))(fa)

  // Implement Functor's map with Monad (flatMap, pure)
  def mapFromMonad[F[_]: Monad, A, B](fa: F[A])(f: A => B): F[B] = Monad[F].flatMap(fa)(Monad[F].pure(_)).map(f)

  // Implement ap with Monad (flatMap, pure, map)
  def apMonad[F[_]: Monad, A, B](ff: F[A => B])(fa: F[A]): F[B] = Monad[F].flatMap(ff)(Monad[F].map(fa)(_))

  // Implement OptionT flatMap, for any F with a Monad.
  def optionFlatMap[F[_]: Monad, A, B](fa: F[Option[A]])(f: A => F[Option[B]]): F[Option[B]] = fa.flatMap {
    case Some(a) => Monad[F].flatMap(Monad[F].pure(a))(f)
    case None => Monad[F].pure(None)
  }
}
