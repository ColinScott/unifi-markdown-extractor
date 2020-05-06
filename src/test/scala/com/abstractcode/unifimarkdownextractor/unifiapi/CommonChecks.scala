package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.data.NonEmptyList
import cats.effect.IO
import com.abstractcode.unifimarkdownextractor.{Fixture, InvalidResponse, TokenUnauthorised, UniFiError}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.AuthCookies
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.{Challenge, HttpRoutes, RequestCookie, Response, Status, headers}
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean

object CommonChecks {
  def checkAuthCookies[R](authCookies: AuthCookies, validResponse: IO[Response[IO]], apiMethod: UniFiApi[IO] => IO[R]): Prop = {
    val expectedCookies = Set(
      RequestCookie("unifises", authCookies.uniFiSes),
      RequestCookie("csrf_token", authCookies.csrfToken)
    )
    val mockServer = HttpRoutes.of[IO] {
      case req@GET -> _ =>
        if (req.cookies.toSet == expectedCookies)
          validResponse
        else
          InternalServerError()
    }.orNotFound

    val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedAppConfiguration)

    propBoolean(apiMethod(httpUniFiApp).attempt.unsafeRunSync().isRight)
  }

  def unauthorised[R](apiMethod: UniFiApi[IO] => IO[R]): Prop = {
    val mockServer = HttpRoutes.of[IO] {
      case GET -> _ => Unauthorized(
        headers.`WWW-Authenticate`(NonEmptyList.of(Challenge("Basic", "test", Map.empty)))
      )
    }.orNotFound

    val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedAppConfiguration)

    propBoolean(apiMethod(httpUniFiApp).attempt.unsafeRunSync() == Left(TokenUnauthorised))
  }

  def unexpectedStatus[R](status: Status, apiMethod: UniFiApi[IO] => IO[R]): Prop = {
    val mockServer = HttpRoutes.of[IO] {
      case GET -> _ => IO.pure(Response[IO](status = status))
    }.orNotFound

    val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedAppConfiguration)

    propBoolean(apiMethod(httpUniFiApp).attempt.unsafeRunSync() == Left(UniFiError(status)))
  }

  def invalidResponseBody[R](notJson: String, apiMethod: UniFiApi[IO] => IO[R]) : Prop = {
    val mockServer = HttpRoutes.of[IO] {
      case GET -> _ => Ok(notJson)
    }.orNotFound

    val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedAppConfiguration)

    propBoolean(apiMethod(httpUniFiApp).attempt.unsafeRunSync() == Left(InvalidResponse))
  }
}
