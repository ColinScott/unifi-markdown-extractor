package com.abstractcode.unifimarkdownextractor.unifiapi

import cats.implicits._
import cats.data.NonEmptyList
import com.abstractcode.unifimarkdownextractor.MarkdownTableConverter
import com.abstractcode.unifimarkdownextractor.MarkdownTableConverter.Column
import com.abstractcode.unifimarkdownextractor.unifiapi.models.LocalNetwork

object MarkdownConversion {
  val localNetworks: NonEmptyList[LocalNetwork] => String = MarkdownTableConverter.convert[LocalNetwork](
    NonEmptyList.of[Column[LocalNetwork]](
      Column("Network", _.name.name),
      Column("VLAN", _.vlan.show),
      Column("Network", _.ipSubnet.toString)
    )
  )
}
