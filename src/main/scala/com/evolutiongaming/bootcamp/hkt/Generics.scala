package com.evolutiongaming.bootcamp.hkt

object Generics {
  case class Triple[+A](x: A, y: A, z: A) {
    // exercise 1
    def zip[B](other: Triple[B]): Triple[(A, B)] = Triple((x, other.x), (y, other.y), (z, other.z))

    // exercise 3 (hard) : fix the definition and implement
    //def set(index: Triple.Index, value: A): Triple[A]
    def set[A1 >: A](index: Triple.Index, value: A1): Triple[A1] = index match {
      case Triple.First => copy(x = value)
      case Triple.Second => copy(y = value)
      case Triple.Third => copy(z = value)
    }

  }

  object Triple {

    // exercise 2
    def fromList[A](elements: List[A]): Option[Triple[A]] = elements match {
      case List(x, y, z) => Some(Triple(x, y, z))
      case _ => None
    }

    sealed trait Index

    case object First extends Index

    case object Second extends Index

    case object Third extends Index
  }

  // R is in results so it got +
  // A is in parameters so it got -
  // M is in both so leave at it is
  trait Walker[-A, M, +R] { // exercise 4 : fill in correct variance annotations
    def init: M

    def next(element: A, previous: M): M

    def stop(last: M): R

    // exercice 5 implement
    def contramap[B](f: B => A): Walker[B, M, R] = new Walker[B, M, R] {
      def init: M = ???

      def next(element: B, previous: M): M = ???

      def stop(last: M): R = ???
    }
  }


  trait Collection[+A] {
    //    def walk(walker: Walker[A, M, R]): R

    //    def map(f: A => B): Collection[B] = ??? // exercise 6 : implement (using Walker.contramap)

    //    def flatMap(f: A => Collection[B]) : Collection[B] = ??? // HomeWork 2 : implement
  }

  object Collection {
    def apply[A](seq: A*): Collection[A] = ??? // Homework 1: implement
  }

}


object Subkinding {

  trait Animal

  class Dog extends Animal

  class Cat extends Animal

  type >:>[+A, -B] = <:<[B, A]
  type ???[A, B] = DummyImplicit

  // sub or super 1
  implicitly[ {
    type T[+_]
  } <:< {
    type T[_]
  }]

  // sub or super 2
  implicitly[ {
    type T[_]
  } >:> {
    type T[-_]
  }]

  // sub or super 3
  implicitly[ {
    type T[_, _]
  } >:> {
    type T[-_, +_]
  }]


  // sub or super 4 - they are incompatible
  implicitly[ {
    type T[_[_]]
  } ??? {
    type T[_]
  }]

  // sub or super 5
  implicitly[ {
    type T[_[_]]
  } <:< {
    type T[_[-_]]
  }]

  // sub or super 6 - they are incompatible
  implicitly[ {
    type T[_[+_]]
  } ??? {
    type T[_[-_]]
  }]

  // sub or super 7
  implicitly[ {
    type T[_[_[+_]]]
  } <:< {
    type T[_[_[_]]]
  }]

  // sub or super 8
  implicitly[ {
    type T[_ >: Dog <: Animal]
  } >:> {
    type T[_]
  }]

  // sub or super 9
  implicitly[ {
    type T[_[_ >: Dog <: Animal]]
  } <:< {
    type T[_[_]]
  }]

  // sub or super 10 - special case that generates scala bugg (fixed in new version)
  implicitly[ {
    type T[_[x] <: Iterable[_]]
  } <:< {
    type T[_[_]]
  }]

}
