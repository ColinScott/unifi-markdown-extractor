package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.unifiapi.models.SitesDetails.Site
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}

case class SitesDetails(data: List[Site])

object SitesDetails {
  case class Site(
    id: String,
    name: String,
    description: String,
    role: String,
    hiddenId: Option[String],
    noDelete: Option[Boolean]
  )

  implicit val encodeSite: Encoder[Site] = Encoder.forProduct6(
    "_id",
    "name",
    "desc",
    "role",
    "attr_hidden_id",
    "attr_no_delete"
  )(s => (s.id, s.name, s.description, s.role, s.hiddenId, s.noDelete))

  implicit val decodeSite: Decoder[Site] = Decoder.forProduct6(
    "_id",
    "name",
    "desc",
    "role",
    "attr_hidden_id",
    "attr_no_delete"
  )(Site.apply)

  implicit val encodeSitesDetails: Encoder[SitesDetails] = deriveEncoder
  implicit val decodeSitesDetails: Decoder[SitesDetails] = deriveDecoder
}