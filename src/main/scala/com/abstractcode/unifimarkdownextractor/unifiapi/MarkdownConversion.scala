package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.implicits._
import cats.data.NonEmptyList
import com.abstractcode.unifimarkdownextractor.MarkdownTableConverter
import com.abstractcode.unifimarkdownextractor.MarkdownTableConverter.Column
import com.abstractcode.unifimarkdownextractor.unifiapi.models.FirewallGroup.{Ipv4AddressSubnetGroup, PortGroup}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{FirewallGroup, LocalNetwork}

object MarkdownConversion {
  val localNetworks: NonEmptyList[LocalNetwork] => String = MarkdownTableConverter.convert[LocalNetwork](
    NonEmptyList.of[Column[LocalNetwork]](
      Column("Network", _.name.name),
      Column("VLAN", _.vlan.show),
      Column("Network", _.ipSubnet.toString)
    )
  )

  val firewallGroups: NonEmptyList[FirewallGroup] => String = MarkdownTableConverter.convert[FirewallGroup](
    NonEmptyList.of[Column[FirewallGroup]](
      Column("Name", _.name),
      Column("Type", {
        case _: PortGroup => "Port Group"
        case _: Ipv4AddressSubnetGroup => "IPv4 Address/Subnet"
        case _ => "Unknown"
      }),
      Column("" , {
        case p: PortGroup => p.members.mkString(" ")
        case i: Ipv4AddressSubnetGroup => i.members.mkString("<br/>")
        case _ => ""
      })
    )
  )
}
