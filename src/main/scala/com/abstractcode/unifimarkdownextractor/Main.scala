package com.abstractcode.unifimarkdownextractor

import java.security.SecureRandom
import java.security.cert.X509Certificate

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.effect._
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.MarkdownTableConverter.Column
import com.abstractcode.unifimarkdownextractor.configuration.{AppConfiguration, ParseError}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, LocalNetwork, Network, Site}
import com.abstractcode.unifimarkdownextractor.unifiapi.{HttpUniFiApi, UniFiApi}
import javax.net.ssl.{SSLContext, X509TrustManager}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext.global

object Main extends IOApp {
  def process(appConfiguration: AppConfiguration, client: Client[IO]): IO[ExitCode] = {
    val uniFiApi = new HttpUniFiApi[IO](client, appConfiguration)
    for {
      authCookies <- uniFiApi.authenticate()
      _ <- IO(println(authCookies))
      sites <- uniFiApi.sites(authCookies)
      _ <- IO(println(sites))
      _ <- showNetworks(uniFiApi, authCookies)(sites)
      _ <- uniFiApi.logout(authCookies)
    } yield ExitCode.Success
  }

  def showNetworks(uniFiApi: UniFiApi[IO], authCookies: AuthCookies)(sites: List[Site]): IO[Unit] = {
    for {
      networksList <- sites.traverse(s => uniFiApi.networks(authCookies)(s.name))
      _ <- networksList.traverse(l => networksToTable(l))
    } yield ()
  }

  def networksToTable(networks: List[Network]): IO[Unit] = {
    val localNetworks: List[LocalNetwork] = networks
      .flatMap(_ match { case l: LocalNetwork => Some(l) case _ => None })
      .sortBy(_.vlan.map(_.id).getOrElse(1: Short))

    val converter: NonEmptyList[LocalNetwork] => String = MarkdownTableConverter.convert[LocalNetwork](
      NonEmptyList.of(
        Column[LocalNetwork]("Network", _.name.name),
        Column[LocalNetwork]("VLAN", _.vlan.map(_.id.toString).getOrElse("1")),
        Column[LocalNetwork]("Network", _.ipSubnet.toString)
      )
    )

   NonEmptyList.fromList(localNetworks)
      .map(n => IO(println(converter(n))))
      .getOrElse(IO.pure(()))
  }

  def showNetworkListList(networksList: List[List[Network]]): IO[Unit] = for {
    networks <- IO.pure(networksList)
    _ <- IO(println(networks))
  } yield ()

  def showNetworkList(networks: List[Network]): IO[Unit] = IO(println(networks))

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
