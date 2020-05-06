package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.Generators._
import io.circe.syntax._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object SitesDetailsSpec extends Properties("SiteDetails JSON") {
  property("can round trip encode and decode") = forAll {
    (sitesDetails: SitesDetails) => sitesDetails.asJson.as[SitesDetails] == Right(sitesDetails)
  }
}

