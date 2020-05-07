package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.unifiapi.models.Identifiers._
import io.circe.{Decoder, Encoder}

object SitesDetails {
  case class Site(
    id: SiteId,
    name: String,
    description: String,
    role: String,
    hiddenId: Option[String],
    noDelete: Option[Boolean]
  )

  implicit val encodeSite: Encoder[Site] = Encoder.forProduct6[Site, SiteId, String, String, String, Option[String], Option[Boolean]](
    "_id",
    "name",
    "desc",
    "role",
    "attr_hidden_id",
    "attr_no_delete"
  )(s => (s.id, s.name, s.description, s.role, s.hiddenId, s.noDelete))

  implicit val decodeSite: Decoder[Site] = Decoder.forProduct6[Site, SiteId, String, String, String, Option[String], Option[Boolean]](
    "_id",
    "name",
    "desc",
    "role",
    "attr_hidden_id",
    "attr_no_delete"
  )(Site.apply)
}