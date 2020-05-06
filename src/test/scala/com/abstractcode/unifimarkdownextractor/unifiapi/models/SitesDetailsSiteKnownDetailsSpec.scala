package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.unifiapi.models.SitesDetails.Site
import io.circe.parser._
import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

class SitesDetailsSiteKnownDetailsSpec extends Specification {
  def is: SpecStructure =
    s2"""
       Sites details site
          should be decoded from known JSON with optional attributes $decodeFromKnownJsonWithOptional
          should be decoded from known JSON without optional attributes $decodeFromKnownJsonWithoutOptional
    """

  val knownContentWithOptional: String =
    """{
      |  "_id" : "12345678",
      |  "name" : "default",
      |  "desc" : "Default",
      |  "attr_hidden_id" : "default",
      |  "attr_no_delete" : true,
      |  "role" : "admin"
      |}""".stripMargin

  val knownContentWithoutOptional: String =
    """{
      |  "_id" : "9876521213",
      |  "desc" : "Other",
      |  "name" : "erhqpnmlh",
      |  "role" : "user"
      |}""".stripMargin

  def decodeFromKnownJsonWithOptional: MatchResult[Any] = {
    val decoded = parse(knownContentWithOptional).flatMap(_.as[Site])
    val expected = Right(Site(
      id = SiteId("12345678"),
      name = "default",
      description = "Default",
      role = "admin",
      hiddenId = Some("default"),
      noDelete = Some(true)
    ))
    decoded shouldEqual expected
  }

  def decodeFromKnownJsonWithoutOptional: MatchResult[Any] = {
    val decoded = parse(knownContentWithoutOptional).flatMap(_.as[Site])
    val expected = Right(Site(
      id = SiteId("9876521213"),
      name = "erhqpnmlh",
      description = "Other",
      role = "user",
      hiddenId = None,
      noDelete = None
    ))
    decoded shouldEqual expected
  }
}
