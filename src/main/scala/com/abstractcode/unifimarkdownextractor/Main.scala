package com.abstractcode.unifimarkdownextractor

import java.nio.file.{FileSystems, Files}
import java.security.SecureRandom
import java.security.cert.X509Certificate

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.effect._
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.configuration.{AppConfiguration, ParseError}
import com.abstractcode.unifimarkdownextractor.exporter.FileSiteExporter
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, Site}
import com.abstractcode.unifimarkdownextractor.unifiapi.{HttpUniFiApi, UniFiApi}
import javax.net.ssl.{SSLContext, X509TrustManager}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext.global

object Main extends IOApp {
  def process(appConfiguration: AppConfiguration, client: Client[IO]): IO[ExitCode] = {
    val uniFiApi = new HttpUniFiApi[IO](client, appConfiguration.controller)
    for {
      authCookies <- uniFiApi.authenticate()
      _ <- IO(println(authCookies))
      sites <- uniFiApi.sites(authCookies)
      _ <- IO(println(sites))
      _ <- exportSites(uniFiApi, authCookies)(sites)
      _ <- uniFiApi.logout(authCookies)
    } yield ExitCode.Success
  }

  def exportSite(site: Site, uniFiApi: UniFiApi[IO], authCookies: AuthCookies): IO[Unit] = {
    val basePath = FileSystems.getDefault.getPath("/tmp", site.name.name)

    for {
      _ <- IO(Files.createDirectory(basePath))
      _ <- new FileSiteExporter[IO](basePath, uniFiApi, authCookies).export(site)
    } yield ()
  }

  def exportSites(uniFiApi: UniFiApi[IO], authCookies: AuthCookies)(sites: List[Site]): IO[Unit] =
    sites.traverse(site => exportSite(site, uniFiApi, authCookies)).map(_ => ())

  def showConfigError(errors: NonEmptyList[ParseError]): IO[Unit] = IO(println(errors))

  override def run(args: List[String]): IO[ExitCode] = {
    val trustingSslContext: SSLContext = {
      val trustManager = new X509TrustManager {
        def getAcceptedIssuers: Array[X509Certificate] = Array.empty

        override def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = {}

        override def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit = {}
      }
      val sslContext = SSLContext.getInstance("TLS")
      sslContext.init(null, Array(trustManager), new SecureRandom)
      sslContext
    }

    BlazeClientBuilder[IO](global)
      .withSslContext(trustingSslContext)
      .withCheckEndpointAuthentication(false)
      .resource.use { client =>
      for {
        env <- IO(sys.env)
        exitCode <- AppConfiguration(env) match {
          case Valid(config) => process(config, client)
          case Invalid(e) => showConfigError(e).map(_ => ExitCode.Error)
        }
      } yield exitCode
    }
  }


}
