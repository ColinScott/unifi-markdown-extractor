package com.abstractcode.unifimarkdownextractor

import com.abstractcode.unifimarkdownextractor.configuration.{AppConfiguration, Credentials}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Network.{NetworkName, VLan}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Site.SiteName
import com.abstractcode.unifimarkdownextractor.unifiapi.models._
import org.http4s.{Status, Uri}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import org.http4s.Status._

object Generators {
  val one = 1 // Work around Scala 2.13.2 bug
  val nonEmptyString: Gen[String] = arbitrary[String].suchThat(!_.isEmpty)
  val nonEmptyOrWhitespaceString: Gen[String] = arbitrary[String].suchThat(!_.trim.isEmpty)
  val whitespaceString: Gen[String] = Gen.chooseNum(one - 1, 32).map(" " * _)
  val hiddenId: Gen[Option[String]] = Gen.option(Gen.identifier)
  val noDelete: Gen[Option[Boolean]] = Gen.option(Gen.oneOf(List(true, false)))

  implicit val uri: Gen[Uri] = for {
    protocol <- Gen.frequency(List((5, "http://"), (10, "https://")).map(freqTuple): _*)
    uri <- Gen.identifier
    port <- Gen.chooseNum[Int](minT = 1, maxT = 65535)
  } yield Uri.unsafeFromString(s"$protocol$uri:$port")
  implicit val arbitraryUri: Arbitrary[Uri] = Arbitrary(uri)

  implicit val credentials: Gen[Credentials] = for {
    username <- nonEmptyOrWhitespaceString
    password <- nonEmptyOrWhitespaceString
  } yield Credentials(username, password)
  implicit val arbitraryCredentials: Arbitrary[Credentials] = Arbitrary(credentials)

  implicit val arbitraryAppConfiguration: Arbitrary[AppConfiguration] = Arbitrary {
    for {
      uri <- uri
      credentials <- credentials
    } yield AppConfiguration(uri, credentials)
  }

  implicit val arbitraryAuthCookies: Arbitrary[AuthCookies] = Arbitrary {
    for {
      uniFiSes <- Gen.alphaStr
      csrfToken <- Gen.alphaStr
    } yield AuthCookies(uniFiSes, csrfToken)
  }

  implicit val siteId: Gen[SiteId] = for {
    id <- Gen.identifier
  } yield SiteId(id)
  implicit val arbitrarySiteId: Arbitrary[SiteId] = Arbitrary(siteId)

  implicit val siteName: Gen[SiteName] = for {
    id <- Gen.identifier
  } yield SiteName(id)
  implicit val arbitrarySiteName: Arbitrary[SiteName] = Arbitrary(siteName)

  implicit val sitesDetailsSite: Gen[Site] = for {
    id <- siteId
    name <- siteName
    description <- Gen.identifier
    role <- Gen.identifier
    hiddenId <- hiddenId
    delete <- noDelete
  } yield Site(id, name, description, role, hiddenId, delete)
  implicit val arbitrarySitesDetailsSite: Arbitrary[Site] = Arbitrary(sitesDetailsSite)

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

  val networkId: Gen[NetworkId] = for {
    id <- Gen.identifier
  } yield NetworkId(id)
  implicit val arbitraryNetworkId: Arbitrary[NetworkId] = Arbitrary(networkId)

  implicit val networkName: Gen[NetworkName] = for {
    id <- Gen.identifier
  } yield NetworkName(id)
  implicit val arbitraryNetworkName: Arbitrary[NetworkName] = Arbitrary(networkName)

  implicit val ipAddressV4: Gen[IpAddressV4] = for {
    a <- arbitrary[Byte]
    b <- arbitrary[Byte]
    c <- arbitrary[Byte]
    d <- arbitrary[Byte]
  } yield IpAddressV4(a, b, c, d)
  implicit val arbitraryIpAddressV4: Arbitrary[IpAddressV4] = Arbitrary(ipAddressV4)

  implicit val cidrV4: Gen[CidrV4] = for {
    ip <- ipAddressV4
    prefixLength <- Gen.choose[Byte]((one - 1).toByte, 32.toByte)
  } yield CidrV4(ip, prefixLength)
  implicit val arbitraryCidrV4: Arbitrary[CidrV4] = Arbitrary(cidrV4)

  implicit val defaultNetwork: Gen[LocalNetwork] = for {
    id <- networkId
    name <- networkName
    ipSubnet <- cidrV4
  } yield LocalNetwork(id, name, None, ipSubnet, Some("LAN"), Some(true))

  implicit val lan: Gen[LocalNetwork] = for {
    id <- networkId
    name <- networkName
    vlan <- Gen.posNum[Short]
    ipSubnet <- cidrV4
  } yield LocalNetwork(id, name, Some(VLan(vlan)), ipSubnet, None, None)

  implicit val wan: Gen[WideAreaNetwork] = for {
    id <- networkId
    name <- networkName
    hiddenId <- Gen.oneOf("WAN", "WAN2")
    delete <- noDelete
  } yield WideAreaNetwork(id, name, Some(hiddenId), delete)

  implicit val arbitraryNetwork: Arbitrary[Network] = Arbitrary(Gen.oneOf(defaultNetwork, lan, wan))
}
