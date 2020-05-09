package com.abstractcode.unifimarkdownextractor

import java.nio.file.{FileSystems, Path}

import com.abstractcode.unifimarkdownextractor.configuration.Credentials
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Network.{NetworkName, VLan}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.Site.SiteName
import com.abstractcode.unifimarkdownextractor.unifiapi.models._
import org.http4s.Uri
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen._

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

  implicit val credentials: Gen[Credentials] = for {
    username <- nonEmptyOrWhitespaceString
    password <- nonEmptyOrWhitespaceString
  } yield Credentials(username, password)

  implicit val siteId: Gen[SiteId] = for {
    id <- Gen.identifier
  } yield SiteId(id)

  implicit val siteName: Gen[SiteName] = for {
    id <- Gen.identifier
  } yield SiteName(id)

  implicit val site: Gen[Site] = for {
    id <- siteId
    name <- siteName
    description <- Gen.identifier
    role <- Gen.identifier
    hiddenId <- hiddenId
    delete <- noDelete
  } yield Site(id, name, description, role, hiddenId, delete)

  val networkId: Gen[NetworkId] = for {
    id <- Gen.identifier
  } yield NetworkId(id)

  implicit val networkName: Gen[NetworkName] = for {
    id <- Gen.identifier
  } yield NetworkName(id)

  implicit val ipAddressV4: Gen[IpAddressV4] = for {
    a <- arbitrary[Byte]
    b <- arbitrary[Byte]
    c <- arbitrary[Byte]
    d <- arbitrary[Byte]
  } yield IpAddressV4(a, b, c, d)

  implicit val cidrV4: Gen[CidrV4] = for {
    ip <- ipAddressV4
    prefixLength <- Gen.choose[Byte]((one - 1).toByte, 32.toByte)
  } yield CidrV4(ip, prefixLength)

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

  implicit val path: Gen[Path] = for {
    count <- Gen.choose(1, 5)
    sections <- Gen.listOfN(count, Gen.alphaNumStr.filter(!_.isEmpty))
  } yield FileSystems.getDefault.getPath(sections.head, sections.tail: _*)
}
