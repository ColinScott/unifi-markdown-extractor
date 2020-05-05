package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.ApplicativeError
import cats.effect._
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.configuration.AppConfiguration
import com.abstractcode.unifimarkdownextractor.unifiapi.AddAuthCookies._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.SitesDetails.Site
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, SitesDetails}
import com.abstractcode.unifimarkdownextractor.{AuthenticationFailure, InvalidAuthenticationResponse}
import fs2.Stream
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.Status.Successful
import org.http4s.client.Client
import org.http4s.{Method, Request, ResponseCookie}
import org.http4s.circe.CirceEntityDecoder._

trait UniFiApi[F[_]] {
  def authenticate(): F[AuthCookies]
  def sites(authCookies: AuthCookies): F[List[Site]]
}

class HttpUniFiApi[F[_] : Sync](client: Client[F], appConfiguration: AppConfiguration)(implicit monadError: ApplicativeError[F, Throwable]) extends UniFiApi[F] {
  def authenticate(): F[AuthCookies] = {
    val postRequest = Request[F](
      method = Method.POST,
      uri = appConfiguration.serverUri / "api" / "login",
      body = Stream.emits[F, Byte](appConfiguration.credentials.asJson.noSpaces.getBytes)
    )

    client.run(postRequest).use { response =>
      response.status.responseClass match {
        case Successful =>
          val getCookies: String => Option[String] = getCookieValue(response.cookies)
          val uniFiSes = getCookies("unifises")
          val csrfToken = getCookies("csrf_token")

          monadError.fromOption(uniFiSes.map2(csrfToken)(AuthCookies), InvalidAuthenticationResponse)
        case _ => monadError.raiseError[AuthCookies](AuthenticationFailure(response.status))
      }
    }
  }

  def sites(authCookies: AuthCookies): F[List[Site]] = {
    val getRequest: Request[F] = Request[F](
      method = Method.GET,
      uri = appConfiguration.serverUri / "api" / "self" / "sites"
    ).addAuthCookies(authCookies)

    for {
      sitesDetails <- client.expect[SitesDetails](getRequest)
    } yield sitesDetails.data
  }

  def getCookieValue(cookies: List[ResponseCookie])(name: String): Option[String] = cookies.filter(_.name == name)
    .map(_.content)
    .headOption

  def addCookies(request: Request[F], authCookies: AuthCookies): Request[F] = request
    .addCookie("unifises", authCookies.uniFiSes)
    .addCookie("csrf_token", authCookies.csrfToken)
}

trait AddAuthCookies[R] {
  def addAuthCookies(request: R, authCookies: AuthCookies): R
}

object AddAuthCookies {
  def apply[R](implicit sh: AddAuthCookies[R]): AddAuthCookies[R] = sh

  implicit class AddAuthCookiesOps[R : AddAuthCookies](val request: R) {
    def addAuthCookies(authCookies: AuthCookies): R = AddAuthCookies[R].addAuthCookies(request, authCookies)
  }

  implicit def ioRequestAddAuthCookies[F[_]]: AddAuthCookies[Request[F]] =
    (request, authCookies) => request.addCookie("unifises", authCookies.uniFiSes).addCookie("csrf_token", authCookies.csrfToken)
}
