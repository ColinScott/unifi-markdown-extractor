package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Site._
import io.circe.syntax._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object SiteSpec extends Properties("Site JSON") {
  property("can round trip encode and decode") = forAll {
    (site: Site) => site.asJson.as[Site] == Right(site)
  }
}

