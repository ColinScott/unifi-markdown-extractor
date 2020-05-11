package com.abstractcode.unifimarkdownextractor.exporter

import java.nio.file.Path

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.configuration.ExportConfiguration
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, FirewallGroup, FirewallRule, LocalNetwork, Network, Site}
import com.abstractcode.unifimarkdownextractor.unifiapi.{MarkdownConversion, UniFiApi}

trait SiteExporter[F[_]] {
  def export(site: Site): F[Unit]
}

class FileSiteExporter[F[_] : Monad : Sync](exportConfiguration: ExportConfiguration, uniFiApi: UniFiApi[F], authCookies: AuthCookies) extends SiteExporter[F] {
  val ruleSets: List[FirewallRule.RuleSet] = List(
    FirewallRule.WAN,
    FirewallRule.LAN,
    FirewallRule.Guest,
    FirewallRule.WANV6,
    FirewallRule.LANV6,
    FirewallRule.GuestV6
  )

  def export(site: Site): F[Unit] = for {
    siteDirectory <- Sync[F].delay(exportConfiguration.basePath.resolve(site.name.name))
    _ <- FileActions.createDirectory[F](siteDirectory)
    networks <- uniFiApi.networks(authCookies)(site.name)
    localNetworks = networks
      .flatMap { case l: LocalNetwork => Some(l) case _ => None }
      .sortBy(_.vlan.map(_.id).getOrElse(1: Short))
    _ <- writeLocalNetworks(siteDirectory, localNetworks)
    firewallGroups <- uniFiApi.firewallGroups(authCookies)(site.name)
    firewallRules <- uniFiApi.firewallRules(authCookies)(site.name)
    _ <- writeFirewallGroups(siteDirectory, firewallGroups)
    rulesWriter = (r: FirewallRule.RuleSet) => writeFirewallRules(siteDirectory, firewallGroups, networks, firewallRules)(r)
    _ <- ruleSets.traverse_(rulesWriter)
  } yield ()

  def writeLocalNetworks(siteDirectory: Path, localNetworks: List[LocalNetwork]): F[Unit] = NonEmptyList.fromList(localNetworks) match {
    case Some(ln) => FileActions.write[F](siteDirectory.resolve("networks.md"), MarkdownConversion.localNetworks(ln))
    case None => Sync[F].pure(())
  }

  def writeFirewallGroups(siteDirectory: Path, firewallGroups: List[FirewallGroup]): F[Unit] = NonEmptyList.fromList(firewallGroups) match {
    case Some(ln) => FileActions.write[F](siteDirectory.resolve("firewall-groups.md"), MarkdownConversion.firewallGroups(ln))
    case None => Sync[F].pure(())
  }

  def writeFirewallRules(siteDirectory: Path, firewallGroups: List[FirewallGroup], networks: List[Network], firewallRules: List[FirewallRule])
    (ruleSet: FirewallRule.RuleSet): F[Unit] =
    NonEmptyList.fromList(firewallRules.filter(_.ruleSet == ruleSet).sortBy(_.index)) match {
      case Some(ln) =>
        val fileName = s"firewall-rules-${ruleSet.show.toLowerCase.replace(' ', '-')}.md"
        FileActions.write[F](siteDirectory.resolve(fileName), MarkdownConversion.firewallRules(firewallGroups, networks)(ln))
      case None => Sync[F].pure(())
    }
}
