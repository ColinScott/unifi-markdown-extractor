package com.abstractcode.unifimarkdownextractor

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.effect._
import com.abstractcode.unifimarkdownextractor.configuration.{AppConfiguration, ParseError}

object Main extends IOApp {
  def process(): IO[ExitCode] = IO.pure(ExitCode.Success)

  def showConfigError(errors: NonEmptyList[ParseError]): IO[Unit] = IO(println(errors))

  override def run(args: List[String]): IO[ExitCode] = for {
    env <- IO(sys.env)
    exitCode <- AppConfiguration(env) match {
      case Valid(_) => process()
      case Invalid(e) => showConfigError(e).map(_ => ExitCode.Error)
    }
  } yield exitCode
}
