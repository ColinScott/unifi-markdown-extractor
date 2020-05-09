package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.effect.IO
import com.abstractcode.unifimarkdownextractor.Error.{AuthenticationFailure, InvalidAuthenticationResponse}
import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.configuration.{ControllerConfiguration, Credentials}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.AuthCookies
import com.abstractcode.unifimarkdownextractor.Fixture
import io.circe.generic.auto._
import org.http4s._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object HttpUniFiApiAuthenticationSpec extends Properties("HttpUniFiApi authentication") {

  property("successful authentication") = forAll {
    (configuration: ControllerConfiguration, authCookies: AuthCookies) => {
      val mockServer = HttpRoutes.of[IO] {
        case req@POST -> Root / "api" / "login" =>
          for {
            credentials <- req.as[Credentials]
            response <- if (credentials == configuration.credentials)
              IO.pure(
                Response[IO](status = Status.Ok)
                  .addCookie("unifises", authCookies.uniFiSes)
                  .addCookie("csrf_token", authCookies.csrfToken)
              )
            else
              InternalServerError()
          } yield response
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer),configuration)

      val receivedCookies = httpUniFiApp.authenticate().unsafeRunSync()

      receivedCookies == authCookies
    }
  }

  property("invalid credentials") = forAll {
    (configuration: ControllerConfiguration) => {
      val mockServer = HttpRoutes.of[IO] {
        case req@POST -> Root / "api" / "login" =>
          for {
            credentials <- req.as[Credentials]
            response <- if (credentials == configuration.credentials)
              BadRequest()
            else
              InternalServerError()
          } yield response
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), configuration)

      val response = httpUniFiApp.authenticate().attempt.unsafeRunSync()

      response match {
        case Left(AuthenticationFailure(s)) if s.code == BadRequest.code => true
        case _ => false
      }
    }
  }

  property("missing unifises cookie") = forAll {
    (authCookies: AuthCookies) => {
      val mockServer = HttpRoutes.of[IO] {
        case POST -> Root / "api" / "login" =>
          IO.pure(
            Response[IO](status = Status.Ok).addCookie("csrf_token", authCookies.csrfToken)
          )
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedControllerConfiguration)

      val response = httpUniFiApp.authenticate().attempt.unsafeRunSync()

      response match {
        case Left(InvalidAuthenticationResponse) => true
        case _ => false
      }
    }
  }

  property("missing csrf_token cookie") = forAll {
    (authCookies: AuthCookies) => {
      val mockServer = HttpRoutes.of[IO] {
        case POST -> Root / "api" / "login" =>
          IO.pure(
            Response[IO](status = Status.Ok).addCookie("unifises", authCookies.uniFiSes)
          )
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedControllerConfiguration)

      val response = httpUniFiApp.authenticate().attempt.unsafeRunSync()

      response match {
        case Left(InvalidAuthenticationResponse) => true
        case _ => false
      }
    }
  }
}

