package com.evolutiongaming.bootcamp.adt

object AlgebraicDataTypes {

  // ALGEBRAIC DATA TYPES

  // Algebraic Data Types or ADTs is a commonly used way of structuring data, used in many programming
  // languages (so this is not something unique to Scala).
  //
  // While the definition may sound scientific and complex, in reality we have already been using this
  // concept in `basics` package. For example, see `sealed trait Shape` in `ClassesAndTraits`. We will now
  // look into the concept and its use cases in more detail.

  // ADTs are widely used in Scala for multiple reasons:
  // - to ensure it is hard or even impossible to represent invalid data;
  // - because pattern matching and ADTs play nicely together.

  // Two common classes of ADTs are:
  // 1. product types: case classes and tuples;
  // 2. sum types: sealed traits and abstract classes.

  // PRODUCT TYPES

  // A product type allows to combine multiple values into one. Canonical Scala examples of product types are
  // case classes and tuples. See `Basics` and `ClassesAndTraits` for their introduction.

  // A product type is called like that because one can calculate how many different values it can possibly
  // have by multiplying the number of such possibilities for the types it combines. The resulting number
  // is called the arity of the product type.

  // Question. What is the arity of the product type `(Boolean, Boolean)`? 4
  type DoubleBoolean = (Boolean, Boolean)

  // Question. What is the arity of the product type `Person`? infinite
  final case class Person(name: String, surname: String, age: Int)

  // Question. `Int`, `Double`, `String`, etc. are useful types from the Scala standard library, which can
  // represent a wide range of data. In the product type `Person`, both the name and the surname are
  // represented by `String`. Is that a good idea?

  // VALUE CLASSES

  // Value classes are a mechanism in Scala to avoid allocating runtime objects, while still providing
  // additional type safety. Runtime objects are not allocated in most cases, but there are notable
  // exceptions, see the following link for more details:
  // https://docs.scala-lang.org/overviews/core/value-classes.html

  // `Age` has a single, public val parameter that is the underlying runtime representation. The type at
  // compile time is `Age`, but at runtime, the representation is `Int`. Case classes can also be used to
  // define value classes, see `Name`.
  class Age(val value: Int) extends AnyVal
  final case class Name(value: String) extends AnyVal {
    def greeting: String = s"Hello, $value!"
  }

  // Type aliases may seem similar to value classes, but they provide no additional type safety. They can,
  // however, increase readability of the code in certain scenarios.
  final case class Surname(value: String) extends AnyVal
  type SurnameAlias = String // No additional type safety in comparison to `String`, arguably a bad example!

  // Question. Can you come up with an example, where using type aliases would make sense?
  // They are good to aggregate data, type PersonByAge = Map[Int, List[Person]]

  // Exercise. Rewrite the product type `Person`, so that it uses value classes.
  final case class PersonName(name: String) extends AnyVal
  final case class PersonSurname(surname: String) extends AnyVal
  final case class PersonAge private (age: Int) extends AnyVal
  final object PersonAge {
    def apply(value: Int): Option[PersonAge] =
      if (value < 0 || value > 120) None
      else Some(new PersonAge(value))
  }
  final case class Person2 (name: PersonName, surname: PersonSurname, age: PersonAge)

  val person: Option[Person2] =
    for (ageV <- PersonAge(5))
      yield Person2(PersonName("Paweł"), PersonSurname("Kędzierski"), ageV)
  println(person) // Some(Person2(PersonName(Paweł),PersonSurname(Kędzierski),PersonAge(5)))
  val person2: Option[Person2] =
    for (ageV <- PersonAge(-2))
      yield Person2(PersonName("Paweł"), PersonSurname("Kędzierski"), ageV)
  println(person2) // None


  // SMART CONSTRUCTORS

  // Smart constructor is a pattern, which allows creating only valid instances of a class.

  // Exercise. Create a smart constructor for `GameLevel` that only permits levels from 1 to 80 (inclusive).
  final case class GameLevel private (value: Int) extends AnyVal
  object GameLevel {
    def create(value: Int): Option[GameLevel] = value match {
      case x if x >= 1 && x <= 80 => Some(GameLevel(x))
      case _ => None
    }
  }

  println(GameLevel(0)) // GameLevel(0)
  //println(new GameLevel(-1)) constructor is private so I can't access it with new keyword

  final case class GameLevel2 private (value: Int) extends AnyVal
  object GameLevel2 {
    def apply(value: Int): Option[GameLevel2] = value match {
      case x if x >= 1 && x <= 80 => Some(new GameLevel2(x))
      case _ => None
    }
  }

  println(GameLevel2(1)) // Some(GameLevel2(1))
  println(GameLevel2(-1)) // None

  // To disable creating case classes in any other way besides smart constructor, the following pattern
  // can be used. However, it is rather syntax-heavy and cannot be combined with value classes.
  sealed abstract case class Time private (hour: Int, minute: Int)
  object Time {
    def create(hour: Int, minute: Int): Either[InvalidValueError, Time] = (hour, minute) match {
      case (h, _) if h < 0 || h > 23 => Left(InvalidHourError)
      case (_, m) if m < 0 || m > 59 => Left(InvalidMinuteError)
      case _ => Right(new Time(hour, minute) {})
    }
  }
  // We have to use {} because we are making new anonymous class (because of abstract)

  // Exercise. Implement the smart constructor for `Time` that only permits values from 00:00 to 23:59 and
  // returns "Invalid hour value" or "Invalid minute value" strings in `Left` when appropriate.

  // Question. Is using `String` to represent `Left` a good idea? Why? no, because it's bad for later working with error
  sealed trait InvalidValueError
  final case object InvalidHourError extends InvalidValueError
  final case object InvalidMinuteError extends InvalidValueError

  // SUM TYPES

  // A sum type is an enumerated type. To define it one needs to enumerate all its possible variants.
  // A custom boolean type `Bool` can serve as a canonical example.
  sealed trait Bool
  object Bool {
    final case object True extends Bool
    final case object False extends Bool
  }

  // Note that sealed keyword means that `Bool` can only be extended in the same file as its declaration.
  // Question. Why do you think sealed keyword is essential to define sum types?

  // A sum type is called like that because one can calculate how many different values it can possibly
  // have by adding the number of such possibilities for the types it enumerates. The resulting number
  // is called the arity of the sum type.

  // Question. What is the arity of the sum type `Bool`? 2

  // The power of sum and product types is unleashed when they are combined together. For example, consider a
  // case where multiple different payment methods need to be supported. (This is an illustrative example and
  // should not be considered complete.)
  final case class AccountNumber(value: String) extends AnyVal
  final case class CardNumber(value: String) extends AnyVal
  final case class ValidityDate(month: Int, year: Int)
  sealed trait PaymentMethod
  object PaymentMethod {
    final case class BankAccount(accountNumber: AccountNumber) extends PaymentMethod
    final case class CreditCard(cardNumber: CardNumber, validityDate: ValidityDate) extends PaymentMethod
    final case object Cash extends PaymentMethod
  }

  import PaymentMethod._

  final case class PaymentStatus(value: String) extends AnyVal
  trait BankAccountService {
    def processPayment(amount: BigDecimal, accountNumber: AccountNumber): PaymentStatus
  }
  trait CreditCardService {
    def processPayment(amount: BigDecimal, creditCard: CreditCard): PaymentStatus
  }
  trait CashService {
    def processPayment(amount: BigDecimal): PaymentStatus
  }

  // Exercise. Implement `PaymentService.processPayment` using pattern matching and ADTs.
  class PaymentService(
    bankAccountService: BankAccountService,
    creditCardService: CreditCardService,
    cashService: CashService,
  ) {
    def processPayment(amount: BigDecimal, method: PaymentMethod): PaymentStatus = method match {
      case BankAccount(accountNumber) => bankAccountService.processPayment(amount, accountNumber)
      case CreditCard(cardNumber, validityDate) => creditCardService.processPayment(amount, CreditCard(cardNumber, validityDate))
      case Cash => cashService.processPayment(amount)
      case _ => PaymentStatus("Payment canceled.")
    }
  }

  // Let's compare that to `NaivePaymentService.processPayment` implementation, which does not use ADTs, but
  // provides roughly the same features as `PaymentService`.
  // Question. What are disadvantages of `NaivePaymentService`? Are there any advantages?
  trait NaivePaymentService { // Obviously a bad example!
    def processPayment(
      amount: BigDecimal,
      bankAccountNumber: Option[String],
      validCreditCardNumber: Option[String],
      isCash: Boolean,
    ): String =
      if (bankAccountNumber.isDefined) s"Paid $amount with bank account ${bankAccountNumber.get}."
      else if (validCreditCardNumber.isDefined) s"Paid $amount with credit card ${validCreditCardNumber.get}."
      else if (isCash) s"Paid $amount in cash."
      else "Payment canceled."
  }

  // Attributions and useful links:
  // https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
  // https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
  // https://en.wikipedia.org/wiki/Algebraic_data_type
}
