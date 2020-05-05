package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.data.NonEmptyList
import cats.effect.IO
import com.abstractcode.unifimarkdownextractor.{Fixture, InvalidResponse, TokenUnauthorised, UniFiError}
import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.configuration.AppConfiguration
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, SitesDetails}
import io.circe.syntax._
import org.http4s.{Challenge, HttpRoutes, RequestCookie, Response, Status, headers}
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object HttpUniFiApiSitesSpec extends Properties("HttpUniFiApi sites") {

  property("get sites") = forAll {
    (sitesDetails: SitesDetails) => {
      val mockServer = HttpRoutes.of[IO] {
        case GET -> Root / "api" / "self" / "sites" => Ok(sitesDetails.asJson)
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedAppConfiguration)

      httpUniFiApp.sites(Fixture.fixedAuthCookies).unsafeRunSync() == sitesDetails.data
    }
  }

  property("send auth cookies") = forAll {
    (authCookies: AuthCookies) => {
      val expectedCookies = Set(
        RequestCookie("unifises", authCookies.uniFiSes),
        RequestCookie("csrf_token", authCookies.csrfToken)
      )

      val mockServer = HttpRoutes.of[IO] {
        case req@GET -> Root / "api" / "self" / "sites" =>
          if (req.cookies.toSet == expectedCookies)
            Ok(SitesDetails(Nil).asJson)
          else
            InternalServerError()
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedAppConfiguration)

      httpUniFiApp.sites(authCookies).unsafeRunSync() == Nil
    }
  }

  property("unauthorised") = forAll {
    (appConfiguration: AppConfiguration) => {
      val mockServer = HttpRoutes.of[IO] {
        case GET -> Root / "api" / "self" / "sites" => Unauthorized(
          headers.`WWW-Authenticate`(NonEmptyList.of(Challenge("Basic", "test", Map.empty)))
        )
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer),appConfiguration)

      httpUniFiApp.sites(Fixture.fixedAuthCookies).attempt.unsafeRunSync() == Left(TokenUnauthorised)
    }
  }

  property("unexpected status code") = forAll {
    (status: Status) => {
      val mockServer = HttpRoutes.of[IO] {
        case GET -> Root / "api" / "self" / "sites" => IO.pure(Response[IO](status = status))
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedAppConfiguration)

      httpUniFiApp.sites(Fixture.fixedAuthCookies).attempt.unsafeRunSync() == Left(UniFiError(status))
    }
  }

  property("bad response") = forAll {
    (notJson: String) => {
      val mockServer = HttpRoutes.of[IO] {
        case GET -> Root / "api" / "self" / "sites" => Ok(notJson)
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedAppConfiguration)

      httpUniFiApp.sites(Fixture.fixedAuthCookies).attempt.unsafeRunSync() == Left(InvalidResponse)
    }
  }
}
