package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.Generators._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Identifiers._
import io.circe.Json
import io.circe.syntax._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object SiteIdSpec extends Properties("SiteId JSON") {
  property("can round trip encode and decode") = forAll {
    (siteId: SiteId) => siteId.asJson.as[SiteId] == Right(siteId)
  }

  property("can decode from string") = forAll(Gen.identifier) {
    (id: String) => Json.fromString(id).as[SiteId] == Right(SiteId(id))
  }
}
