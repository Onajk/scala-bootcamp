package com.evolutiongaming.bootcamp.effects.v3

import cats.effect.{BracketThrow, ExitCase, ExitCode, IO, IOApp, Sync}
import cats.syntax.all._

import scala.concurrent.duration._
import scala.io.{BufferedSource, Source}

object GuaranteeApp extends IOApp {

  def sleepy(msg: String): IO[Unit] = IO.sleep(1.second) *> IO(println(msg))

  def withGuaranteeCase[A](io: IO[A]): IO[A] =
    io.guaranteeCase {
      case ExitCase.Completed    => IO.delay(println("Completed"))
      case ExitCase.Canceled     => IO.delay(println("Canceled"))
      case ExitCase.Error(error) => IO.delay(println(s"Error: ${error.getMessage}"))
    }

  def cancelledProgram: IO[Unit] =
    for {
      fiber <- withGuaranteeCase(sleepy("Not gonna finish")).start
      _     <- IO.sleep(500.millis)
      _     <- fiber.cancel
    } yield ()

  def run(args: List[String]): IO[ExitCode] =
    cancelledProgram.as(ExitCode.Success)
}

object BracketApp extends IOApp {

  def acquire(name: String): IO[BufferedSource] = IO(Source.fromFile(name))
  def release(source: BufferedSource): IO[Unit] = IO(source.close())

  def readSource(source: BufferedSource): IO[Iterator[String]] = IO(source.getLines())

  def bracketProgram: IO[Unit] =
    acquire("ReadMe.md")
      .bracket { bufferedSource =>
        readSource(bufferedSource)
          .map(_.mkString("\n"))
          .flatMap(str => IO.delay(println(str)))
      } { bufferedSource =>
        release(bufferedSource) // <- this operation is concurrent with `use` section in case of cancellation
      }

  def acquireF[F[_]: Sync](name: String): F[BufferedSource] = Sync[F].delay(Source.fromFile(name))
  def releaseF[F[_]: Sync](source: BufferedSource): F[Unit] = Sync[F].delay(source.close())
  def readSourceF[F[_]: Sync](source: BufferedSource): F[Iterator[String]] = Sync[F].delay(source.getLines())

  def bracketProgramF[F[_]: Sync: BracketThrow]: IO[Unit] =
    acquire("ReadMe.md")
      .bracket { bufferedSource =>
        readSource(bufferedSource)
          .map(_.mkString("\n"))
          .flatMap(str => IO.delay(println(str)))
      } { bufferedSource =>
        release(bufferedSource) // <- this operation is concurrent with `use` section in case of cancellation
      }

  def run(args: List[String]): IO[ExitCode] =
    //bracketProgram.as(ExitCode.Success)
    //bracketProgramF[IO].as(ExitCode.Success)
    IO.delay(print("Resource"))
      .bracket { _ =>
        IO.raiseError(new RuntimeException("Error in use"))
    } { _ =>
        IO.raiseError(new RuntimeException("Error in release"))
      }
      .handleErrorWith{ e => IO.delay(println(e.getMessage)) }
      .as(ExitCode.Success)
}
