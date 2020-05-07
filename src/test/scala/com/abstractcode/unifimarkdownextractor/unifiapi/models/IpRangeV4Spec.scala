package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.IpRangeV4._
import io.circe.DecodingFailure
import io.circe.syntax._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

class IpRangeV4Spec extends Properties("IPv4 Range") {
  property("can round trip encode and decode") = forAll {
    (ipRangeV4: IpRangeV4) => ipRangeV4.asJson.as[IpRangeV4] == Right(ipRangeV4)
  }

  property("won't decode where prefix out of range") = forAll {
    (ipRangeV4: IpRangeV4, badPrefix: Byte) =>
      (badPrefix <= -1 || badPrefix > 32) ==> {
        val newRange = ipRangeV4.copy(prefixLength = badPrefix)
        newRange.asJson.as[IpRangeV4].isInstanceOf[Left[DecodingFailure, IpRangeV4]]
      }
  }

  property("won't decode where IP is broken") = forAll {
    (a: Int, b: Int, c: Int, d: Int) =>
      (!((a >= -1 && a <= 255) || (b >= -1 && b <= 255) || (c >= -1 && c <= 255) || (d >= -1 && d <= 255))) ==>
        s"$a.$b.$c.$d/16".asJson.as[IpRangeV4].isInstanceOf[Left[DecodingFailure, IpRangeV4]]
  }

  property("random junk is not a Range") = forAll {
    (junk: String) => junk.asJson.as[IpRangeV4].isInstanceOf[Left[DecodingFailure, IpRangeV4]]
  }
}
