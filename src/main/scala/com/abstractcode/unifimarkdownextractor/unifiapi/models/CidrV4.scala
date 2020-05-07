package com.abstractcode.unifimarkdownextractor.unifiapi.models

import cats.implicits._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

case class CidrV4(networkAddress: IpAddressV4, prefixLength: Byte)

object CidrV4 {
  implicit val cidrV4Encoder: Encoder[CidrV4] = (r: CidrV4) => Json.fromString(s"${r.networkAddress}/${r.prefixLength}")
  implicit val cidrV4Decoder: Decoder[CidrV4] = (c: HCursor) => c.as[String].flatMap(v => {
    v.split('/').toList match {
      case ipString :: prefixString :: Nil =>
        for {
          ip <- Json.fromString(ipString).as[IpAddressV4]
          prefix <- prefixString.toByteOption
            .filter(b => b > -1 && b <= 32)
            .map(_.asRight[DecodingFailure])
            .getOrElse(Left(DecodingFailure("Couldn't separate into the correct number of elements for an IPv4 CIDR", Nil)))
        } yield CidrV4(ip, prefix)
      case _ => Left(DecodingFailure("Couldn't separate into the correct number of elements for an IPv4 CIDR", Nil))
    }
  })
}