package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.unifiapi.models.Identifiers._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

case class Network(id: NetworkId)

object Network {

  implicit val encodeNetwork: Encoder[Network] = (a: Network) => Json.obj(
    ("_id", a.id.asJson)
  )
  implicit val decodeNetwork: Decoder[Network] = (c: HCursor) => for {
    id <- c.downField("_id").as[NetworkId]
  } yield Network(id)

  implicit val encodeNetworksResponse: Encoder[UniFiResponse[List[Network]]] = deriveEncoder
  implicit val decodeNetworksResponse: Decoder[UniFiResponse[List[Network]]] = deriveDecoder
}