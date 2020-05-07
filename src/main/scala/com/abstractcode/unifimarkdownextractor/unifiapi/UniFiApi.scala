package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.ApplicativeError
import cats.effect._
import cats.implicits._
import com.abstractcode.unifimarkdownextractor._
import com.abstractcode.unifimarkdownextractor.configuration.AppConfiguration
import com.abstractcode.unifimarkdownextractor.infrastructure.AddAuthCookies._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Site.SiteName
import com.abstractcode.unifimarkdownextractor.unifiapi.models._
import fs2.Stream
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.Status.{ClientError, Successful}
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.client.Client
import org.http4s.{Method, Request, Response, ResponseCookie}

trait UniFiApi[F[_]] {
  def authenticate(): F[AuthCookies]
  def logout(authCookies: AuthCookies): F[Unit]

  def sites(authCookies: AuthCookies): F[List[Site]]
  def networks(authCookies: AuthCookies)(name: SiteName): F[List[Network]]
}

class HttpUniFiApi[F[_] : Sync](client: Client[F], appConfiguration: AppConfiguration)(implicit monadError: ApplicativeError[F, Throwable]) extends UniFiApi[F] {
  def authenticate(): F[AuthCookies] = {
    def getCookieValue(cookies: List[ResponseCookie])(name: String): Option[String] = cookies.filter(_.name == name)
      .map(_.content)
      .headOption

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

  def logout(authCookies: AuthCookies): F[Unit] = {
    val request: Request[F] = Request[F](
      method = Method.POST,
      uri = appConfiguration.serverUri / "api" / "logout"
    )

    handleWithAuthentication(request, authCookies, _.as[UniFiResponse[Unit]])
  }

  def sites(authCookies: AuthCookies): F[List[Site]] = {
    val request: Request[F] = Request[F](
      method = Method.GET,
      uri = appConfiguration.serverUri / "api" / "self" / "sites"
    )

    handleWithAuthentication(
      request,
      authCookies,
      _.as[UniFiResponse[List[Site]]]
    )
  }

  def networks(authCookies: AuthCookies)(name: SiteName): F[List[Network]] = {
    val request: Request[F] = Request[F](
      method = Method.GET,
      uri = appConfiguration.serverUri / "api" / "s" / name.name / "rest" / "networkconf"
    )

    handleWithAuthentication(
      request,
      authCookies,
      _.as[UniFiResponse[List[Network]]]
    )
  }

  def handleWithAuthentication[R](
    request: Request[F],
    authCookies: AuthCookies,
    success: Response[F] => F[UniFiResponse[R]]
  ): F[R] = {
    client.run(request.addAuthCookies(authCookies)).use { response =>
      response.status.responseClass match {
        case Successful => success(response).map(_.data).handleErrorWith(_ => monadError.raiseError(InvalidResponse))
        case ClientError if response.status.code == 401 => monadError.raiseError[R](TokenUnauthorised)
        case _ => monadError.raiseError[R](UniFiError(response.status))
      }
    }
  }
}


