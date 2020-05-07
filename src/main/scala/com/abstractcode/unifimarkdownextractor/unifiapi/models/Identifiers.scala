package com.abstractcode.unifimarkdownextractor.unifiapi.models

import io.circe.{Decoder, Encoder, HCursor, Json}

case class SiteId(id: String)
case class NetworkId(id: String)

object Identifiers {
  implicit val encodeSiteId: Encoder[SiteId] = (a: SiteId) => Json.fromString(a.id)
  implicit val decodeSiteId: Decoder[SiteId] = (c: HCursor) => c.as[String].map(SiteId)
  implicit val encodeNetworkId: Encoder[NetworkId] = (a: NetworkId) => Json.fromString(a.id)
  implicit val decodeNetworkId: Decoder[NetworkId] = (c: HCursor) => c.as[String].map(NetworkId)
}