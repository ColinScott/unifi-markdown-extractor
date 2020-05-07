package com.abstractcode.unifimarkdownextractor

import com.abstractcode.unifimarkdownextractor.configuration.{AppConfiguration, Credentials}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Site.SiteName
import com.abstractcode.unifimarkdownextractor.unifiapi.models._
import org.http4s.{Status, Uri}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.freqTuple
import org.scalacheck.{Arbitrary, Gen}
import org.http4s.Status._

object Generators {
  val one = 1 // Work around Scala 2.13.2 bug
  val nonEmptyString: Gen[String] = arbitrary[String].suchThat(!_.isEmpty)
  val nonEmptyOrWhitespaceString: Gen[String] = arbitrary[String].suchThat(!_.trim.isEmpty)
  val whitespaceString: Gen[String] = Gen.chooseNum(one - 1, 32).map(" " * _)

  implicit val uriGen: Gen[Uri] = for {
    protocol <- Gen.frequency(List((5, "http://"), (10, "https://")).map(freqTuple): _*)
    uri <- Gen.identifier
    port <- Gen.chooseNum[Int](minT = 1, maxT = 65535)
  } yield Uri.unsafeFromString(s"$protocol$uri:$port")
  implicit val arbitraryUri: Arbitrary[Uri] = Arbitrary(uriGen)

  implicit val credentialsGen: Gen[Credentials] = for {
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
      uniFiSes <- Gen.alphaStr
      csrfToken <- Gen.alphaStr
    } yield AuthCookies(uniFiSes, csrfToken)
  }

  implicit val siteIdGen: Gen[SiteId] = for {
    id <- Gen.identifier
  } yield SiteId(id)
  implicit val arbitrarySiteId: Arbitrary[SiteId] = Arbitrary(siteIdGen)

  implicit val siteNameGen: Gen[SiteName] = for {
    id <- Gen.identifier
  } yield SiteName(id)
  implicit val arbitrarySiteName: Arbitrary[SiteName] = Arbitrary(siteNameGen)

  val networkIdGen: Gen[NetworkId] = for {
    id <- Gen.identifier
  } yield NetworkId(id)
  implicit val arbitraryNetworkId: Arbitrary[NetworkId] = Arbitrary(networkIdGen)

  implicit val sitesDetailsSiteGen: Gen[Site] = for {
    id <- siteIdGen
    name <- siteNameGen
    description <- Gen.identifier
    role <- Gen.identifier
    hiddenId <- Gen.option(Gen.identifier)
    noDelete <- Gen.option(Gen.oneOf(List(true, false)))
  } yield Site(id, name, description, role, hiddenId, noDelete)
  implicit val arbitrarySitesDetailsSite: Arbitrary[Site] = Arbitrary(sitesDetailsSiteGen)

  implicit def arbitraryUniFiResponse[T](implicit tGen: Gen[T]): Arbitrary[UniFiResponse[List[T]]] = Arbitrary {
    for {
      t <- Gen.listOf(tGen)
    } yield UniFiResponse(t)
  }

  implicit val arbitraryStatus: Arbitrary[Status] = Arbitrary {
    Gen.oneOf(
      Forbidden,
      Gone,
      NotImplemented,
      GatewayTimeout,
      VariantAlsoNegotiates,
      UpgradeRequired
    )
  }

  implicit val networkGen: Gen[Network] = for {
    id <- networkIdGen
  } yield Network(id)
  implicit val arbitraryNetwork: Arbitrary[Network] = Arbitrary(networkGen)
}
