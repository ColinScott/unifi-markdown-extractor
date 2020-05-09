package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.effect.IO
import com.abstractcode.unifimarkdownextractor.Arbitraries._
import com.abstractcode.unifimarkdownextractor.Fixture
import com.abstractcode.unifimarkdownextractor.unifiapi.CommonChecks._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.AuthCookies
import io.circe.Json
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.{HttpRoutes, Status}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object HttpUniFiApiLogoutSpec extends Properties("HttpUniFiApi authentication") {

  property("successful logout") = forAll {
    (authCookies: AuthCookies) => {
      val mockServer = HttpRoutes.of[IO] {
        case POST -> Root / "api" / "logout" => Ok(Json.obj("data" -> Json.arr()))
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedControllerConfiguration)

      httpUniFiApp.logout(authCookies).attempt.unsafeRunSync() == Right(())
    }
  }

  property("send auth cookies") = forAll {
    (authCookies: AuthCookies) => checkAuthCookies(
      authCookies,
      Ok(Json.obj("data" -> Json.arr())),
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
