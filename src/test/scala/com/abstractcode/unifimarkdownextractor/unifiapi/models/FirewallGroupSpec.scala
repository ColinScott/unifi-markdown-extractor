package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.Arbitraries._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.FirewallGroup._
import io.circe.syntax._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object FirewallGroupSpec extends Properties("FirewallGroup") {
  property("can round trip encode and decode") = forAll {
    (firewallGroup: FirewallGroup) => firewallGroup.asJson.as[FirewallGroup] == Right(firewallGroup)
  }
}
