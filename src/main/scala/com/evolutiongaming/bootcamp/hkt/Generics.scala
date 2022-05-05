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

    // exercise 5 implement
    def contramap[B](f: B => A): Walker[B, M, R] = new Walker[B, M, R] {
      def init: M = Walker.this.init

      def next(element: B, previous: M): M = Walker.this.next(f(element), previous)

      def stop(last: M): R = Walker.this.stop(last)
    }
  }


  trait Collection[+A] {
    def walk[M, R](walker: Walker[A, M, R]): R

    // exercise 6 : implement
    def map[B](f: A => B): Collection[B] = new Collection[B] {
      def walk[M, R](walker: Walker[B, M, R]): R = Collection.this.walk(walker.contramap(f))
    }

    // HomeWork 2 : implement
    //def flatMap[B](f: A => Collection[B]) : Collection[B] = ???
  }

  object Collection {
    // Homework 1: implement
    def apply[A](seq: A*): Collection[A] = new Collection[A] {
      def walk[M, R](walker: Walker[A, M, R]): R =
        walker.stop(
          seq.foldLeft(walker.init)((acc, elem) => walker.next(elem, acc))
        )
    }
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
