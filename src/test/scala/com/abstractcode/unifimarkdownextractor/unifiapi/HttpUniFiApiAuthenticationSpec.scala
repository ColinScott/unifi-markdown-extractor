package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.effect.IO
import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.configuration.{AppConfiguration, Credentials}
import com.abstractcode.unifimarkdownextractor.unifiapi.UniFiApi.AuthCookies
import io.circe.generic.auto._
import org.http4s._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Properties}

object HttpUniFiApiAuthenticationSpec extends Properties("HttpUniFiApi authentication") {

  implicit val arbitraryAuthCookies: Arbitrary[AuthCookies] = Arbitrary {
    for {
      uniFiSes <- nonEmptyString
      csrfToken <- nonEmptyString
    } yield AuthCookies(uniFiSes, csrfToken)
  }

  property("successful authentication") = forAll {
    (appConfiguration: AppConfiguration, authCookies: AuthCookies) => {
      val mockServer = HttpRoutes.of[IO] {
        case req @ POST -> Root / "api" / "login" =>
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

      val client = Client.fromHttpApp(mockServer)

      val httpUniFiApp = new HttpUniFiApi[IO](client)

      val receivedCookies = httpUniFiApp.authenticate(appConfiguration).unsafeRunSync()

      receivedCookies == authCookies
    }
  }
}

