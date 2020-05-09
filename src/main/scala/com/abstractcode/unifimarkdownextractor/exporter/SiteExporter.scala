package com.abstractcode.unifimarkdownextractor.exporter

import java.nio.file.Path

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.configuration.ExportConfiguration
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, LocalNetwork, Site}
import com.abstractcode.unifimarkdownextractor.unifiapi.{MarkdownConversion, UniFiApi}

trait SiteExporter[F[_]] {
  def export(site: Site): F[Unit]
}

class FileSiteExporter[F[_] : Monad : Sync](exportConfiguration: ExportConfiguration, uniFiApi: UniFiApi[F], authCookies: AuthCookies) extends SiteExporter[F] {
  def export(site: Site): F[Unit] = for {
    siteDirectory <- Sync[F].delay(exportConfiguration.basePath.resolve(site.name.name))
    _ <- FileActions.createDirectory[F](siteDirectory)
    networks <- uniFiApi.networks(authCookies)(site.name)
    localNetworks = networks
      .flatMap { case l: LocalNetwork => Some(l) case _ => None }
      .sortBy(_.vlan.map(_.id).getOrElse(1: Short))
    _ <- writeLocalNetworks(siteDirectory, localNetworks)
  } yield ()

  def writeLocalNetworks(siteDirectory: Path, localNetworks: List[LocalNetwork]): F[Unit] = NonEmptyList.fromList(localNetworks) match {
    case Some(ln) => FileActions.write[F](siteDirectory.resolve("networks.md"), MarkdownConversion.localNetworks(ln))
    case None => Sync[F].pure(())
  }
}
