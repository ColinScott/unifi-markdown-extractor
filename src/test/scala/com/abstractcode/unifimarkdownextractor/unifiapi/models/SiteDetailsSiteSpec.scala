package com.abstractcode.unifimarkdownextractor.unifiapi.models

import org.scalacheck.Properties
import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.SitesDetails.Site
import org.scalacheck.Prop.{forAll, propBoolean}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.SitesDetails._
import io.circe.syntax._

object SiteDetailsSiteSpec extends Properties("SiteDetails.Site JSON") {
  property("can round trip encode and decode") = forAll {
    (site: Site) => site.asJson.as[Site] == Right(site)
  }
}

