package com.abstractcode.unifimarkdownextractor.unifiapi.models

sealed trait FirewallGroup

object FirewallGroup {
  case class PortGroup(id: FirewallGroupId,
    name: String,
    members: List[Int],
    siteId: SiteId
  ) extends FirewallGroup
  case class Ipv4AddressSubnetGroup(
    id: FirewallGroupId,
    name: String,
    members: List[IPv4],
    siteId: SiteId
  ) extends FirewallGroup
  case class UnknownGroupType(name: String) extends FirewallGroup
}