package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.ApplicativeError
import cats.effect._
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.{AuthenticationFailure, InvalidAuthenticationResponse}
import com.abstractcode.unifimarkdownextractor.configuration.AppConfiguration
import com.abstractcode.unifimarkdownextractor.unifiapi.UniFiApi.AuthCookies
import fs2.Stream
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.Status.Successful
import org.http4s.client._
import org.http4s.{Method, Request, ResponseCookie}

trait UniFiApi[F[_]] {
  def authenticate(appConfiguration: AppConfiguration): F[AuthCookies]
}

class HttpUniFiApi[F[_] : Sync](client: Client[F])(implicit monadError: ApplicativeError[F, Throwable]) extends UniFiApi[F] {
  def authenticate(appConfiguration: AppConfiguration): F[AuthCookies] = {
    val postRequest = Request[F](
      method = Method.POST,
      uri = appConfiguration.serverUri / "api" / "login",
      body = Stream.emits[F, Byte](appConfiguration.credentials.asJson.noSpaces.getBytes)
    )

    client.run(postRequest).use { response =>
      response.status.responseClass match {
        case Successful => {
          val getCookies: String => Option[String] = getCookieValue(response.cookies)
          val uniFiSes = getCookies("unifises")
          val csrfToken = getCookies("csrf_token")

          monadError.fromOption(uniFiSes.map2(csrfToken)(AuthCookies), InvalidAuthenticationResponse)
        }
        case _ => monadError.raiseError[AuthCookies](AuthenticationFailure(response.status))
      }
    }
  }

  def getCookieValue(cookies: List[ResponseCookie])(name: String): Option[String] = cookies.filter(_.name == name)
    .map(_.content)
    .headOption
}

object UniFiApi {

  case class AuthCookies(uniFiSes: String, csrfToken: String)

}