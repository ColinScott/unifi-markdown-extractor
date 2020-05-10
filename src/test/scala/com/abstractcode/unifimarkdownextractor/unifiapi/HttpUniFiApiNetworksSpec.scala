package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.effect.IO
import com.abstractcode.unifimarkdownextractor.Arbitraries._
import com.abstractcode.unifimarkdownextractor.unifiapi.CommonChecks._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Network._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Site._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.UniFiResponse._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, Network, UniFiResponse}
import com.abstractcode.unifimarkdownextractor.{Fixture, Generators}
import io.circe.Json
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.{HttpRoutes, Status}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Properties}

object HttpUniFiApiNetworksSpec extends Properties("HttpUniFiApi firewall groups") {
  implicit val network: Gen[Network] = Generators.network

  property("get sites") = forAll {
    (siteName: SiteName, networks: UniFiResponse[List[Network]]) => {
      val mockServer = HttpRoutes.of[IO] {
        case GET -> Root / "api" / "s" / siteName.name / "rest" / "networkconf" => Ok(networks.asJson)
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedControllerConfiguration)

      httpUniFiApp.networks(Fixture.fixedAuthCookies)(siteName).unsafeRunSync() == networks.data
    }
  }

  property("send auth cookies") = forAll {
    (siteName: SiteName, authCookies: AuthCookies) => checkAuthCookies(
      authCookies,
      Ok(Json.obj("data" -> Json.arr())),
      _.networks(authCookies)(siteName)
    )
  }

  property("unauthorised") = forAll {
    (siteName: SiteName, authCookies: AuthCookies) => unauthorised(_.networks(authCookies)(siteName))
  }

  property("unexpected status code") = forAll {
    (siteName: SiteName, status: Status) => unexpectedStatus(status, _.networks(Fixture.fixedAuthCookies)(siteName))
  }

  property("invalid response body") = forAll {
    (siteName: SiteName, notJson: String) => invalidResponseBody(notJson, _.networks(Fixture.fixedAuthCookies)(siteName))
  }
}
