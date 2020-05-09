package com.abstractcode.unifimarkdownextractor.exporter

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Path

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.{Resource, Sync}
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, LocalNetwork, Site}
import com.abstractcode.unifimarkdownextractor.unifiapi.{MarkdownConversion, UniFiApi}

trait SiteExporter[F[_]] {
  def export(site: Site): F[Unit]
}

class FileSiteExporter[F[_] : Monad : Sync](basePath: Path, uniFiApi: UniFiApi[F], authCookies: AuthCookies) extends SiteExporter[F] {
  def export(site: Site): F[Unit] = for {
    networks <- uniFiApi.networks(authCookies)(site.name)
    localNetworks = networks
      .flatMap { case l: LocalNetwork => Some(l) case _ => None }
      .sortBy(_.vlan.map(_.id).getOrElse(1: Short))
    _ <- writeLocalNetworks(localNetworks)
  } yield ()

  def writeLocalNetworks(localNetworks: List[LocalNetwork]): F[Unit] = NonEmptyList.fromList(localNetworks) match {
    case Some(ln) => write(basePath, "networks.md", MarkdownConversion.localNetworks(ln))
    case None => Sync[F].pure(())
  }

  def write(basePath: Path, filename: String, content: String): F[Unit] = for {
    file <- Sync[F].delay(basePath.resolve(filename).toFile)
    _ <- writer(file).use { w => Sync[F].delay(w.write(content)) }
  } yield ()

  def writer(file: File): Resource[F, BufferedWriter] = Resource.fromAutoCloseable(
    Sync[F].delay(new BufferedWriter(new FileWriter(file)))
  )
}
