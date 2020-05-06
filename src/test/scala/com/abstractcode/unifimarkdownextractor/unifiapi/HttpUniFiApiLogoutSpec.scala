package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.effect.IO
import com.abstractcode.unifimarkdownextractor.Fixture
import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.unifiapi.CommonChecks._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.AuthCookies
import org.http4s.{HttpRoutes, Status}
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object HttpUniFiApiLogoutSpec extends Properties("HttpUniFiApi authentication") {

  property("successful logout") = forAll {
    (authCookies: AuthCookies) => {
      val mockServer = HttpRoutes.of[IO] {
        case GET -> Root / "api" / "logout" => Ok()
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedAppConfiguration)

      httpUniFiApp.logout(authCookies).attempt.unsafeRunSync() == Right(())
    }
  }

  property("send auth cookies") = forAll {
    (authCookies: AuthCookies) => checkAuthCookies(
      authCookies,
      Ok(),
      _.logout(authCookies)
    )
  }

  property("unauthorised") = forAll {
    (authCookies: AuthCookies) => unauthorised(_.logout(authCookies))
  }

  property("unexpected status code") = forAll {
    (status: Status) => unexpectedStatus(status, _.logout(Fixture.fixedAuthCookies))
  }
}
