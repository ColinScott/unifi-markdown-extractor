package com.abstractcode.unifimarkdownextractor.configuration

import cats.data._
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.configuration.ParseError._
import org.http4s.Uri
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.freqTuple
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Properties}

object AppConfigurationSpec extends Properties("AppConfiguration") {

  val one = 1 // Work around Scala 2.13.2 bug
  val nonEmptyOrWhitespaceString: Gen[String] = arbitrary[String].suchThat(!_.trim.isEmpty)
  val whitespaceString: Gen[String] = Gen.chooseNum(one - 1, 32).map(" " * _)

  implicit val arbitraryUri: Arbitrary[Uri] = Arbitrary {
    for {
      protocol <- Gen.frequency(List((5, "http://"), (10, "https://")).map(freqTuple): _*)
      uri <- Gen.identifier
      port <- Gen.chooseNum[Int](minT = 1, maxT = 65535)
    } yield Uri.unsafeFromString(s"$protocol$uri:$port")
  }
  implicit val arbitraryCredentials: Arbitrary[Credentials] = Arbitrary {
    for {
      username <- nonEmptyOrWhitespaceString
      password <- nonEmptyOrWhitespaceString
    } yield Credentials(username, password)
  }

  val badUsernameCredentials: Gen[Credentials] = for {
    username <- whitespaceString
    password <- nonEmptyOrWhitespaceString
  } yield Credentials(username, password)

  val badPasswordCredentials: Gen[Credentials] = for {
    username <- nonEmptyOrWhitespaceString
    password <- whitespaceString
  } yield Credentials(username, password)

  val entirelyBadCredentials: Gen[Credentials] = for {
    username <- whitespaceString
    password <- whitespaceString
  } yield Credentials(username, password)

  property("valid") = forAll {
    (uri: Uri, credentials: Credentials) => {
      val env = Map(
        "SERVER_URI" -> uri.toString(),
        "USERNAME" -> credentials.username,
        "PASSWORD" -> credentials.password
      )

      val expected = AppConfiguration(uri, credentials).validNel

      AppConfiguration(env) == expected
    }
  }

  property("invalid uri") = forAll {
    (credentials: Credentials) => {
      val env = Map(
        "SERVER_URI" -> "$#^$%&(%",
        "USERNAME" -> credentials.username,
        "PASSWORD" -> credentials.password
      )

      val expected = ParseError("SERVER_URI", InvalidFormat).invalidNel

      AppConfiguration(env) == expected
    }
  }

  property("missing uri") = forAll {
    (credentials: Credentials) => {
      val env = Map(
        "USERNAME" -> credentials.username,
        "PASSWORD" -> credentials.password
      )

      val expected = ParseError("SERVER_URI", NotProvidedOrEmpty).invalidNel

      AppConfiguration(env) == expected
    }
  }

  property("bad username") = forAll(arbitraryUri.arbitrary, badUsernameCredentials) {
    (uri: Uri, credentials: Credentials) => {
      val env = Map(
        "SERVER_URI" -> uri.toString(),
        "USERNAME" -> credentials.username,
        "PASSWORD" -> credentials.password
      )

      val expected = ParseError("USERNAME", NotProvidedOrEmpty).invalidNel

      AppConfiguration(env) == expected
    }
  }

  property("bad password") = forAll(arbitraryUri.arbitrary, badPasswordCredentials) {
    (uri: Uri, credentials: Credentials) => {
      val env = Map(
        "SERVER_URI" -> uri.toString(),
        "USERNAME" -> credentials.username,
        "PASSWORD" -> credentials.password
      )

      val expected = ParseError("PASSWORD", NotProvidedOrEmpty).invalidNel

      AppConfiguration(env) == expected
    }
  }

  property("entirely bad credentials") = forAll(arbitraryUri.arbitrary, entirelyBadCredentials) {
    (uri: Uri, credentials: Credentials) => {
      val env = Map(
        "SERVER_URI" -> uri.toString(),
        "USERNAME" -> credentials.username,
        "PASSWORD" -> credentials.password
      )

      val expected: ValidatedNel[ParseError, AppConfiguration] = NonEmptyList.of(ParseError("USERNAME", NotProvidedOrEmpty), ParseError("PASSWORD", NotProvidedOrEmpty)).invalid

      AppConfiguration(env) == expected
    }
  }

  property("missing username") = forAll {
    (uri: Uri, credentials: Credentials) => {
      val env = Map(
        "SERVER_URI" -> uri.toString(),
        "PASSWORD" -> credentials.password
      )

      val expected: ValidatedNel[ParseError, AppConfiguration] = ParseError("USERNAME", NotProvidedOrEmpty).invalidNel

      AppConfiguration(env) == expected
    }
  }

  property("missing password") = forAll {
    (uri: Uri, credentials: Credentials) => {
      val env = Map(
        "SERVER_URI" -> uri.toString(),
        "USERNAME" -> credentials.username
      )

      val expected: ValidatedNel[ParseError, AppConfiguration] = ParseError("PASSWORD", NotProvidedOrEmpty).invalidNel

      AppConfiguration(env) == expected
    }
  }
}