package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.effect.IO
import com.abstractcode.unifimarkdownextractor.Fixture
import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.unifiapi.CommonChecks._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.SitesDetails._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, UniFiResponse}
import io.circe.Json
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.{HttpRoutes, Status}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object HttpUniFiApiSitesSpec extends Properties("HttpUniFiApi sites") {

  property("get sites") = forAll {
    (sites: UniFiResponse[List[Site]]) => {
      val mockServer = HttpRoutes.of[IO] {
        case GET -> Root / "api" / "self" / "sites" => Ok(sites.asJson)
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedAppConfiguration)

      httpUniFiApp.sites(Fixture.fixedAuthCookies).unsafeRunSync() == sites.data
    }
  }

  property("send auth cookies") = forAll {
    (authCookies: AuthCookies) => checkAuthCookies(
      authCookies,
      Ok(Json.obj("data" -> Json.arr())),
      _.sites(authCookies)
    )
  }

  property("unauthorised") = forAll {
    (authCookies: AuthCookies) => unauthorised(_.sites(authCookies))
  }

  property("unexpected status code") = forAll {
    (status: Status) => unexpectedStatus(status, _.sites(Fixture.fixedAuthCookies))
  }

  property("invalid response body") = forAll {
    (notJson: String) => invalidResponseBody(notJson, _.sites(Fixture.fixedAuthCookies))
  }
}
