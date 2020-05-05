package com.abstractcode.unifimarkdownextractor

import com.abstractcode.unifimarkdownextractor.configuration.{AppConfiguration, Credentials}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.{AuthCookies, SitesDetails}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.SitesDetails.Site
import org.http4s.Uri
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.freqTuple
import org.scalacheck.{Arbitrary, Gen}

object Generators {
  val one = 1 // Work around Scala 2.13.2 bug
  val nonEmptyString: Gen[String] = arbitrary[String].suchThat(!_.isEmpty)
  val nonEmptyOrWhitespaceString: Gen[String] = arbitrary[String].suchThat(!_.trim.isEmpty)
  val whitespaceString: Gen[String] = Gen.chooseNum(one - 1, 32).map(" " * _)

  val uriGen: Gen[Uri] = for {
    protocol <- Gen.frequency(List((5, "http://"), (10, "https://")).map(freqTuple): _*)
    uri <- Gen.identifier
    port <- Gen.chooseNum[Int](minT = 1, maxT = 65535)
  } yield Uri.unsafeFromString(s"$protocol$uri:$port")
  implicit val arbitraryUri: Arbitrary[Uri] = Arbitrary(uriGen)

  val credentialsGen: Gen[Credentials] = for {
    username <- nonEmptyOrWhitespaceString
    password <- nonEmptyOrWhitespaceString
  } yield Credentials(username, password)
  implicit val arbitraryCredentials: Arbitrary[Credentials] = Arbitrary(credentialsGen)

  implicit val arbitraryAppConfiguration: Arbitrary[AppConfiguration] = Arbitrary {
    for {
      uri <- uriGen
      credentials <- credentialsGen
    } yield AppConfiguration(uri, credentials)
  }

  implicit val arbitraryAuthCookies: Arbitrary[AuthCookies] = Arbitrary {
    for {
      uniFiSes <- nonEmptyString
      csrfToken <- nonEmptyString
    } yield AuthCookies(uniFiSes, csrfToken)
  }

  val sitesDetailsSiteGen: Gen[Site] = for {
    id <- Gen.identifier
    name <- Gen.identifier
    description <- Gen.identifier
    role <- Gen.identifier
    hiddenId <- Gen.option(Gen.identifier)
    noDelete <- Gen.option(Gen.oneOf(List(true, false)))
  } yield Site(id, name, description, role, hiddenId, noDelete)
  implicit val arbitrarySitesDetailsSite: Arbitrary[Site] = Arbitrary(sitesDetailsSiteGen)

  implicit val arbitrarySitesDetails: Arbitrary[SitesDetails] = Arbitrary {
    for {
      sites <- Gen.listOf(sitesDetailsSiteGen)
    } yield SitesDetails(sites)
  }
}
