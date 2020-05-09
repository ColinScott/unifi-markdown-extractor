package com.abstractcode.unifimarkdownextractor.unifiapi.models

import cats.implicits._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

sealed trait IPv4

case class IpAddressV4(a: Byte, b: Byte, c: Byte, d: Byte) extends IPv4 {
  override def toString: String = s"${a & 0xff}.${b & 0xff}.${c & 0xff}.${d & 0xff}"
}

object IpAddressV4 {
  implicit val encodeIpAddressV4: Encoder[IpAddressV4] = (a: IpAddressV4) => Json.fromString(a.toString)
  implicit val decodeIpAddressV4: Decoder[IpAddressV4] = (c: HCursor) => c.as[String].flatMap(ip => {
    ip.split('.').toList.traverse(p => p.toShortOption.map(_.toByte)) match {
      case Some(a :: b :: c :: d :: Nil) =>
        Right(IpAddressV4(a, b, c, d))
      case _ => Left(DecodingFailure("Couldn't turn value into an IPv4 Address", Nil))
    }
  })
}

case class CidrV4(networkAddress: IpAddressV4, prefixLength: Byte) {
  override def toString: String = s"$networkAddress/$prefixLength"
}

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