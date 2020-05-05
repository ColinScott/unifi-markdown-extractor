package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.effect.IO
import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.configuration.{AppConfiguration, Credentials}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.AuthCookies
import com.abstractcode.unifimarkdownextractor.{AuthenticationFailure, InvalidAuthenticationResponse}
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
    (appConfiguration: AppConfiguration, authCookies: AuthCookies) => {
      val mockServer = HttpRoutes.of[IO] {
        case req@POST -> Root / "api" / "login" =>
          for {
            credentials <- req.as[Credentials]
            response <- if (credentials == appConfiguration.credentials)
              IO.pure(
                Response[IO](status = Status.Ok)
                  .addCookie("unifises", authCookies.uniFiSes)
                  .addCookie("csrf_token", authCookies.csrfToken)
              )
            else
              InternalServerError()
          } yield response
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), appConfiguration)

      val receivedCookies = httpUniFiApp.authenticate().unsafeRunSync()

      receivedCookies == authCookies
    }
  }

  property("invalid credentials") = forAll {
    (appConfiguration: AppConfiguration) => {
      val mockServer = HttpRoutes.of[IO] {
        case req@POST -> Root / "api" / "login" =>
          for {
            credentials <- req.as[Credentials]
            response <- if (credentials == appConfiguration.credentials)
              BadRequest()
            else
              InternalServerError()
          } yield response
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), appConfiguration)

      val response = httpUniFiApp.authenticate().attempt.unsafeRunSync()

      response match {
        case Left(AuthenticationFailure(s)) if s.code == BadRequest.code => true
        case _ => false
      }
    }
  }

  property("missing unifises cookie") = forAll {
    (appConfiguration: AppConfiguration, authCookies: AuthCookies) => {
      val mockServer = HttpRoutes.of[IO] {
        case req@POST -> Root / "api" / "login" =>
          for {
            credentials <- req.as[Credentials]
            response <- if (credentials == appConfiguration.credentials)
              IO.pure(
                Response[IO](status = Status.Ok).addCookie("csrf_token", authCookies.csrfToken)
              )
            else
              InternalServerError()
          } yield response
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), appConfiguration)

      val response = httpUniFiApp.authenticate().attempt.unsafeRunSync()

      response match {
        case Left(InvalidAuthenticationResponse) => true
        case _ => false
      }
    }
  }

  property("missing csrf_token cookie") = forAll {
    (appConfiguration: AppConfiguration, authCookies: AuthCookies) => {
      val mockServer = HttpRoutes.of[IO] {
        case req@POST -> Root / "api" / "login" =>
          for {
            credentials <- req.as[Credentials]
            response <- if (credentials == appConfiguration.credentials)
              IO.pure(
                Response[IO](status = Status.Ok).addCookie("unifises", authCookies.uniFiSes)
              )
            else
              InternalServerError()
          } yield response
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), appConfiguration)

      val response = httpUniFiApp.authenticate().attempt.unsafeRunSync()

      response match {
        case Left(InvalidAuthenticationResponse) => true
        case _ => false
      }
    }
  }
}

