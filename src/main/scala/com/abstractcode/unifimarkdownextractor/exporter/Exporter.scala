package com.abstractcode.unifimarkdownextractor.exporter

import cats._
import cats.effect._
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.configuration.ExportConfiguration
import com.abstractcode.unifimarkdownextractor.unifiapi.UniFiApi
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, Site}

trait Exporter[F[_]] {
  def export: F[Unit]
}

class FileExporter[F[_] : Monad : Sync](uniFiApi: UniFiApi[F], exportConfiguration: ExportConfiguration) extends Exporter[F] {
  def export: F[Unit] = for {
    _ <- FileActions.createDirectory[F](exportConfiguration.basePath)
    authCookies <- uniFiApi.authenticate()
    sites <- uniFiApi.sites(authCookies)
    _ <- exportSites(authCookies)(sites)
    _ <- uniFiApi.logout(authCookies)
  } yield ()

  def exportSites(authCookies: AuthCookies)(sites: List[Site]): F[Unit] =
    sites.traverse_(site => exportSite(site, authCookies))

  def exportSite(site: Site, authCookies: AuthCookies): F[Unit] =
    new FileSiteExporter[F](exportConfiguration, uniFiApi, authCookies).export(site)
}