package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.unifiapi.models.Identifiers._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Network._
import io.circe.Decoder.Result
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

sealed trait Network

case class LocalNetwork(
  id: NetworkId,
  name: NetworkName,
  vlan: Option[VLan],
  hiddenId: Option[String],
  noDelete: Option[Boolean]
) extends Network

case class WideAreaNetwork(
  id: NetworkId,
  name: NetworkName,
  hiddenId: Option[String],
  noDelete: Option[Boolean]
) extends Network

object Network {

  case class NetworkName(name: String)

  implicit val encodeSiteName: Encoder[NetworkName] = (a: NetworkName) => Json.fromString(a.name)
  implicit val decodeSiteName: Decoder[NetworkName] = (c: HCursor) => c.as[String].map(NetworkName)

  case class VLan(id: Short)

  implicit val encodeVLan: Encoder[VLan] = (a: VLan) => Json.fromInt(a.id.toInt)
  implicit val decodeVLan: Decoder[VLan] = (c: HCursor) => c.as[Short].map(VLan)

  implicit val encodeNetwork: Encoder[Network] = {
    case WideAreaNetwork(id, name, hiddenId, noDelete) => Json.obj(
      ("_id", id.asJson),
      ("name", name.asJson),
      ("attr_hidden_id", hiddenId.asJson),
      ("attr_no_delete", noDelete.asJson)
    )
    case LocalNetwork(id, name, vlan, hiddenId, noDelete) => Json.obj(
      ("_id", id.asJson),
      ("name", name.asJson),
      ("vlan_enabled", vlan.isDefined.asJson),
      ("vlan", vlan.map(_.id.asJson).getOrElse(Json.fromString(""))),
      ("attr_hidden_id", hiddenId.asJson),
      ("attr_no_delete", noDelete.asJson)
    )
  }

  implicit val decodeNetwork: Decoder[Network] = (c: HCursor) => for {
    hiddenId <- c.downField("attr_hidden_id").as[Option[String]]
    network <- hiddenId match {
      case Some("WAN") | Some("WAN2") => for {
        id <- c.downField("_id").as[NetworkId]
        name <- c.downField("name").as[NetworkName]
        hiddenId <- c.downField("attr_hidden_id").as[Option[String]]
        noDelete <- c.downField("attr_no_delete").as[Option[Boolean]]
      } yield WideAreaNetwork(id, name, hiddenId, noDelete)

      case _ => for {
        id <- c.downField("_id").as[NetworkId]
        name <- c.downField("name").as[NetworkName]
        vlan <- extractVLan(c)
        hiddenId <- c.downField("attr_hidden_id").as[Option[String]]
        noDelete <- c.downField("attr_no_delete").as[Option[Boolean]]
      } yield LocalNetwork(id, name, vlan, hiddenId, noDelete)
    }
  } yield network

  def extractVLan(c: HCursor): Result[Option[VLan]] = for {
    enabled <- c.downField("vlan_enabled").as[Boolean]
    vlan <- if (enabled) c.downField("vlan").as[Option[VLan]] else Right(None)
  } yield vlan
}