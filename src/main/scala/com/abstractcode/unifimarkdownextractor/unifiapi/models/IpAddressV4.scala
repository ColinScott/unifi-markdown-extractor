package com.abstractcode.unifimarkdownextractor.unifiapi.models

import cats.implicits._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

case class IpAddressV4(a: Byte, b: Byte, c: Byte, d: Byte) {
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