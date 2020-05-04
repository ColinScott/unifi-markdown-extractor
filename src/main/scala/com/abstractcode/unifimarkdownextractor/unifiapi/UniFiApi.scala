package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.effect._
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.configuration.AppConfiguration
import fs2.Stream
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.client._
import org.http4s.{Method, Request}

trait UniFiApi[F[_]] {
  def authenticate(appConfiguration: AppConfiguration): F[Unit]
}

class HttpUniFiApi[F[_] : Sync](client: Client[F]) extends UniFiApi[F] {
  def authenticate(appConfiguration: AppConfiguration): F[Unit] = {
    val postRequest = Request[F](
      method = Method.POST,
      uri = appConfiguration.serverUri / "api" / "login",
      body = Stream.emits[F, Byte](appConfiguration.credentials.asJson.noSpaces.getBytes)
    )

    client.expect[Json](postRequest)
      .flatMap(r => Sync[F].delay(println(r)))
      .map(_ => ())
  }
}