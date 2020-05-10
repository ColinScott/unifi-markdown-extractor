package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.effect.IO
import com.abstractcode.unifimarkdownextractor.Arbitraries._
import com.abstractcode.unifimarkdownextractor.{Fixture, Generators}
import com.abstractcode.unifimarkdownextractor.unifiapi.CommonChecks._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.FirewallGroup._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Site._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.UniFiResponse._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, FirewallGroup, UniFiResponse}
import io.circe.Json
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.{HttpRoutes, Status}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Properties}

object HttpUniFiApiFirewallGroupsSpec extends Properties("HttpUniFiApi firewall groups") {
  implicit val firewallGroup: Gen[FirewallGroup] = Generators.firewallGroup

  property("get sites") = forAll {
    (siteName: SiteName, firewallGroups: UniFiResponse[List[FirewallGroup]]) => {
      val mockServer = HttpRoutes.of[IO] {
        case GET -> Root / "api" / "s" / siteName.name / "rest" / "firewallgroup" => Ok(firewallGroups.asJson)
      }.orNotFound

      val httpUniFiApp = new HttpUniFiApi[IO](Client.fromHttpApp(mockServer), Fixture.fixedControllerConfiguration)

      httpUniFiApp.firewallGroups(Fixture.fixedAuthCookies)(siteName).unsafeRunSync() == firewallGroups.data
    }
  }

  property("send auth cookies") = forAll {
    (siteName: SiteName, authCookies: AuthCookies) => checkAuthCookies(
      authCookies,
      Ok(Json.obj("data" -> Json.arr())),
      _.firewallGroups(authCookies)(siteName)
    )
  }

  property("unauthorised") = forAll {
    (siteName: SiteName, authCookies: AuthCookies) => unauthorised(_.firewallGroups(authCookies)(siteName))
  }

  property("unexpected status code") = forAll {
    (siteName: SiteName, status: Status) => unexpectedStatus(status, _.firewallGroups(Fixture.fixedAuthCookies)(siteName))
  }

  property("invalid response body") = forAll {
    (siteName: SiteName, notJson: String) => invalidResponseBody(notJson, _.firewallGroups(Fixture.fixedAuthCookies)(siteName))
  }
}
