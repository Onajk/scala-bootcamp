package com.evolutiongaming.bootcamp.basics

import scala.io.Source

object ControlStructuresHomework2 {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  sealed trait Result
  final case class ChangeMe(value: String) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    // implement this method
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).

    x.split(" ").toList.map(_.trim) match {
      case Nil => Left(ErrorMessage("Empty input."))
      case x :: xs => x match {
        case "divide" => xs.map(_.toDouble) match {
          case dividend :: divisor :: Nil => Right(Command.Divide(dividend, divisor))
          case _ => Left(ErrorMessage("Wrong number of inputs."))
        }
        case "sum" => Right(Command.Sum(xs.map(_.toDouble)))
        case "average" => Right(Command.Average(xs.map(_.toDouble)))
        case "min" => Right(Command.Min(xs.map(_.toDouble)))
        case "max" => Right(Command.Max(xs.map(_.toDouble)))
        case _ => Left(ErrorMessage("No such command."))
      }
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Command.Divide(_, divisor) if divisor == 0 => Left(ErrorMessage("Division by zero."))
      case Command.Divide(dividend, divisor) => Right(ChangeMe(s"$dividend divided by $divisor is ${dividend/divisor}"))
      case Command.Sum(numbers) => Right(ChangeMe(s"the sum of ${numbers.mkString(" ")} is ${numbers.sum}"))
      case Command.Average(numbers) => Right(ChangeMe(s"the average of ${numbers.mkString(" ")} is ${numbers.sum/numbers.length}"))
      case Command.Min(numbers) => Right(ChangeMe(s"the minimum of ${numbers.mkString(" ")} is ${numbers.min}"))
      case Command.Max(numbers) => Right(ChangeMe(s"the maximum of ${numbers.mkString(" ")} is ${numbers.max}"))
    }
  }

  def renderResult(x: Result): String = {
    x match {
      case ChangeMe(s) => s
    }
  }

  def process(x: String): String = {
    import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    // implement using a for-comprehension

    parseCommand(x) match {
      case Left(ErrorMessage(e)) => e
      case Right(command) => calculate(command) match {
        case Left(ErrorMessage(e)) => e
        case Right(result) => renderResult(result)
      }
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
