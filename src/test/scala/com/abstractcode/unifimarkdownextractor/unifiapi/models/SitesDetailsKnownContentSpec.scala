package com.abstractcode.unifimarkdownextractor.unifiapi.models

import com.abstractcode.unifimarkdownextractor.unifiapi.models.SitesDetails.Site
import io.circe.parser._
import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

class SitesDetailsKnownContentSpec extends Specification { def is: SpecStructure =
  s2"""
       Sites details
          should be decoded from known JSON $decodeFromKnownJson
    """

  val knownContent: String =
    """{
      |  "meta" : {
      |    "rc" : "ok"
      |  },
      |  "data" : [
      |    {
      |      "_id" : "12345678",
      |      "name" : "default",
      |      "desc" : "Default",
      |      "attr_hidden_id" : "default",
      |      "attr_no_delete" : true,
      |      "role" : "admin"
      |    },
      |    {
      |      "_id" : "9876521213",
      |      "desc" : "Other",
      |      "name" : "erhqpnmlh",
      |      "role" : "user"
      |    }
      |  ]
      |}""".stripMargin

  def decodeFromKnownJson: MatchResult[Any] = {
    val decoded = parse(knownContent).flatMap(_.as[SitesDetails])
    val expected = Right(
      SitesDetails(
        List(
          Site(SiteId("12345678"), "default", "Default", "admin", Some("default"), Some(true)),
          Site(SiteId("9876521213"), "erhqpnmlh", "Other", "user", None, None)
        )
      )
    )
    decoded shouldEqual expected
  }
}
