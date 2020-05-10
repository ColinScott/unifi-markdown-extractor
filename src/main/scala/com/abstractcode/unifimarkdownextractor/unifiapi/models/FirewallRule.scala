package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.unifiapi.models.FirewallRule._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Identifiers._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import io.circe.syntax._
import cats.implicits._

case class FirewallRule(
  id: FirewallRuleId,
  siteId: SiteId,
  name: String,
  index: Int,
  ruleset: RuleSet,
  ruleSubset: RuleSubset,
  action: Action,
  source: Source,
  destination: Destination,
  enabled: Boolean,
  advancedOptions: Set[AdvancedOptions],
  ipSecMatching: IpSecMatching
)

object FirewallRule {
  sealed trait RuleSet

  case object WAN extends RuleSet
  case object LAN extends RuleSet
  case object Guest extends RuleSet
  case object WANV6 extends RuleSet
  case object LANV6 extends RuleSet
  case object GuestV6 extends RuleSet

  implicit val ruleSetDecoder: Decoder[RuleSet] = (c: HCursor) =>
    c.as[String].map(_.split('_').toList).flatMap {
      case r :: _ :: Nil => r match {
        case "WAN" => Right(WAN)
        case "LAN" => Right(LAN)
        case "GUEST" => Right(Guest)
        case "WANv6" => Right(WANV6)
        case "LANv6" => Right(LANV6)
        case "GUESTv6" => Right(GuestV6)
        case _ => Left(DecodingFailure("Unknown ruleset", Nil))
      }
      case _ => Left(DecodingFailure("Ruleset format invalid", Nil))
    }

  sealed trait RuleSubset
  case object In extends RuleSubset
  case object Out extends RuleSubset
  case object Local extends RuleSubset

  implicit val ruleSubsetDecoder: Decoder[RuleSubset] = (c: HCursor) =>
    c.as[String].map(_.split('_').toList).flatMap {
      case _ :: s :: Nil => s match {
        case "IN" => Right(In)
        case "OUT" => Right(Out)
        case "LOCAL" => Right(Local)
        case _ => Left(DecodingFailure("Unknown ruleset direction", Nil))
      }
      case _ => Left(DecodingFailure("Ruleset format invalid", Nil))
    }

  sealed trait Action

  case object Accept extends Action
  case object Drop extends Action
  case object Reject extends Action

  sealed trait NetworkType

  case object IPv4Subnet extends NetworkType
  case object GatewayIPAddress extends NetworkType

  sealed trait Source

  case class SourceAddressPortGroup(firewallGroups: List[FirewallGroupId], macAddress: Option[String]) extends Source
  case class SourceNetwork(
    network: NetworkId,
    networkType: NetworkType,
    macAddress: Option[String]
  ) extends Source
  case class SourceIPv4Address(ipAddressV4: IpAddressV4, macAddress: Option[String]) extends Source

  sealed trait Destination

  case class DestinationAddressPortGroup(firewallGroups: List[FirewallGroupId]) extends Destination
  case class DestinationNetwork(network: NetworkId, networkType: NetworkType) extends Destination
  case class DestinationIPv4Address(ipAddressV4: IpAddressV4) extends Destination

  sealed trait AdvancedOptions

  case object EnableLogging extends AdvancedOptions
  case object MatchStateNew extends AdvancedOptions
  case object MatchStateEstablished extends AdvancedOptions
  case object MatchStateInvalid extends AdvancedOptions
  case object MatchStateRelated extends AdvancedOptions

  sealed trait IpSecMatching

  case object DontMatchIpSec extends IpSecMatching
  case object MatchInboundIpSec extends IpSecMatching
  case object MatchInboundNonIpSec extends IpSecMatching

  implicit val encodeFirewallRuleAction: Encoder[Action] = {
    case Accept => "accept".asJson
    case Drop => "drop".asJson
    case Reject => "reject".asJson
  }

  implicit val decodeFirewallRuleAction: Decoder[Action] = (c: HCursor) => c.as[String].flatMap {
    case "accept" => Right(Accept)
    case "drop" => Right(Drop)
    case "reject" => Right(Reject)
    case _ => Left(DecodingFailure("Unknown Firewall Rule Action", Nil))
  }

  implicit val encodeIpSecMatching: Encoder[IpSecMatching] = {
    case DontMatchIpSec => "".asJson
    case MatchInboundIpSec => "match-ipsec".asJson
    case MatchInboundNonIpSec => "match-none".asJson
  }

  implicit val decodeIpSecMatching: Decoder[IpSecMatching] = (c: HCursor) => c.as[String].flatMap {
    case "match-ipsec" => Right(MatchInboundIpSec)
    case "match-none" => Right(MatchInboundNonIpSec)
    case "" => Right(DontMatchIpSec)
    case _ => Left(DecodingFailure("Unknown IP Sec Action", Nil))
  }

  implicit val encodeNetworkType: Encoder[NetworkType] = {
    case IPv4Subnet => "NETv4".asJson
    case GatewayIPAddress => "ADDRv4".asJson
  }

  implicit val decodeNetworkType: Decoder[Option[NetworkType]] = (c: HCursor) => c.as[String].flatMap {
    case "NETv4" => Right(Some(IPv4Subnet))
    case "ADDRv4" => Right(Some(GatewayIPAddress))
    case "" => Right(None)
    case _ => Left(DecodingFailure("Unknown Network Type", Nil))
  }

  implicit val encodeFirewallRule: Encoder[FirewallRule] = (r: FirewallRule) => Json.obj(
    ("_id", r.id.asJson),
    ("action", r.action.asJson),
    (
      "dst_firewallgroup_ids",
      (r.destination match {
        case DestinationAddressPortGroup(groups) => groups
        case _ => Nil
      }).asJson
    ),
    ("enabled", r.enabled.asJson),
    ("ipsec", r.ipSecMatching.asJson),
    ("name", r.advancedOptions.contains(EnableLogging).asJson),
    ("name", r.name.asJson),
    ("ruleset", {
      val major = r.ruleset match {
        case WAN => "WAN"
        case LAN => "LAN"
        case Guest => "GUEST"
        case WANV6 => "WANv6"
        case LANV6 => "LANv6"
        case GuestV6 => "GUESTv6"
      }
      val minor = r.ruleSubset match {
        case In => "IN"
        case Out => "OUT"
        case Local => "LOCAL"
      }

      s"${major}_$minor".asJson
    }),
    (
      "src_firewallgroup_ids",
      (r.source match {
        case SourceAddressPortGroup(groups, _) => groups
        case _ => Nil
      }).asJson
    ),
    (
      "src_mac_address",
      (r.source match {
        case SourceAddressPortGroup(_, Some(mac)) => mac
        case SourceNetwork(_, _, Some(mac)) => mac
        case SourceIPv4Address(_, Some(mac)) => mac
        case _ => ""
      }).asJson
    ),
    ("state_established", r.advancedOptions.contains(MatchStateEstablished).asJson),
    ("state_invalid", r.advancedOptions.contains(MatchStateInvalid).asJson),
    ("state_new", r.advancedOptions.contains(MatchStateNew).asJson),
    ("state_related", r.advancedOptions.contains(MatchStateRelated).asJson),
    (
      "dst_address",
      r.destination match {
        case DestinationIPv4Address(address) => address.asJson
        case _ => "".asJson
      }
    ),
    (
      "dst_networkconf_id",
      r.destination match {
        case DestinationNetwork(network, _) => network.asJson
        case _ => "".asJson
      }
    ),
    (
      "dst_networkconf_type",
      r.destination match {
        case DestinationNetwork(_, networkType) => networkType.asJson
        case _ => "".asJson
      }
    ),
    (
      "src_address",
      r.source match {
        case SourceIPv4Address(address, _) => address.asJson
        case _ => "".asJson
      }
    ),
    (
      "src_networkconf_id",
      r.source match {
        case SourceNetwork(network, _, _) => network.asJson
        case _ => "".asJson
      }
    ),
    (
      "src_networkconf_type",
      r.source match {
        case SourceNetwork(_, networkType, _) => networkType.asJson
        case _ => "".asJson
      }
    ),
    ("rule_index", r.index.asJson),
    ("logging", r.advancedOptions.contains(EnableLogging).asJson),
    ("site_id", r.siteId.asJson)
  )

  implicit val decodeFirewallRule: Decoder[FirewallRule] = (c: HCursor) => for {
    id <- c.downField("_id").as[FirewallRuleId]
    siteId <- c.downField("site_id").as[SiteId]
    name <- c.downField("name").as[String]
    index <- c.downField("rule_index").as[Int]
    rulesetParts = c.downField("ruleset")
    ruleset <- rulesetParts.as[RuleSet]
    ruleSubset <- rulesetParts.as[RuleSubset]
    action <- c.downField("action").as[Action]
    source <- for {
      firewallGroupIds <- c.downField("src_firewallgroup_ids").as[List[FirewallGroupId]]
      network <- c.downField("src_networkconf_id").as[String].map(n => if (n.isEmpty) None else Some(NetworkId(n)))
      networkType <- c.downField("src_networkconf_type").as[Option[NetworkType]]
      address <- c.downField("src_address").as[IpAddressV4].map(a => Some(a)).recover {
        case _ => None
      }
      macAddress <- c.downField("src_mac_address").as[String].map(m => if (m.isEmpty) None else Some(m))
      s <- (firewallGroupIds, network, networkType, address) match {
        case (_, Some(n), Some(nt), _) => Right(SourceNetwork(n, nt, macAddress))
        case (_, _, _, Some(a)) => Right(SourceIPv4Address(a, macAddress))
        case (xs, _, _, _) => Right(SourceAddressPortGroup(xs, macAddress))
      }
    } yield s
    destination <- for {
      firewallGroupIds <- c.downField("dst_firewallgroup_ids").as[List[FirewallGroupId]]
      network <- c.downField("dst_networkconf_id").as[String].map(n => if (n.isEmpty) None else Some(NetworkId(n)))
      networkType <- c.downField("dst_networkconf_type").as[Option[NetworkType]]
      address <- c.downField("dst_address").as[IpAddressV4].map(a => Some(a)).recover {
        case _ => None
      }
      d <- (firewallGroupIds, network, networkType, address) match {
        case (_, Some(n), Some(nt), _) => Right(DestinationNetwork(n, nt))
        case (_, _, _, Some(a)) => Right(DestinationIPv4Address(a))
        case (xs, _, _, _) => Right(DestinationAddressPortGroup(xs))
      }
    } yield d
    enabled <- c.downField("enabled").as[Boolean]
    advancedOptions <- for {
      logging <- c.downField("logging").as[Boolean].map(toOption(_, EnableLogging))
      stateNew <- c.downField("state_new").as[Boolean].map(toOption(_, MatchStateNew))
      stateEstablished <- c.downField("state_established").as[Boolean].map(toOption(_, MatchStateEstablished))
      stateInvalid <- c.downField("state_invalid").as[Boolean].map(toOption(_, MatchStateInvalid))
      stateRelated <- c.downField("state_related").as[Boolean].map(toOption(_, MatchStateRelated))
    } yield (logging :: stateNew :: stateEstablished :: stateInvalid :: stateRelated :: Nil).flatten.toSet
    ipSecMatching <- c.downField("ipsec").as[IpSecMatching]
  } yield FirewallRule(
    id,
    siteId,
    name,
    index,
    ruleset,
    ruleSubset,
    action,
    source,
    destination,
    enabled,
    advancedOptions,
    ipSecMatching
  )

  def toOption(flag: Boolean, option: AdvancedOptions): Option[AdvancedOptions] =
    if (flag) Some(option) else None
}
