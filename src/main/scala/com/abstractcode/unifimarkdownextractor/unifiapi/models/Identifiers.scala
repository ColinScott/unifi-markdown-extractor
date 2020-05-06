package com.abstractcode.unifimarkdownextractor.unifiapi.models

import io.circe.{Decoder, Encoder, HCursor, Json}

case class SiteId(id: String)

object Identifiers {
  implicit val encodeSitesId: Encoder[SiteId] = (a: SiteId) => Json.fromString(a.id)
  implicit val decodeSitesId: Decoder[SiteId] = (c: HCursor) => for {
    id <- c.as[String]
  } yield SiteId(id)
}