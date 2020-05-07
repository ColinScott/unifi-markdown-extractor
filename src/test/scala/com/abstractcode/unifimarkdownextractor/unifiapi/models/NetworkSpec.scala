package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Network._
import io.circe.syntax._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object NetworkSpec extends Properties("Network JSON") {
  property("can round trip encode and decode") = forAll {
    (network: Network) => network.asJson.as[Network] == Right(network)
  }
}