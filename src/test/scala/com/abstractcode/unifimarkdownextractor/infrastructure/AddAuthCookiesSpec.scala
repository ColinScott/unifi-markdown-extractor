package com.abstractcode.unifimarkdownextractor.infrastructure

import cats.Id
import com.abstractcode.unifimarkdownextractor.Arbitraries._
import com.abstractcode.unifimarkdownextractor.infrastructure.AddAuthCookies._
import com.abstractcode.unifimarkdownextractor.unifiapi.models.AuthCookies
import org.http4s._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object AddAuthCookiesSpec extends Properties("AddAuthCookies") {

  property("addAuthCookies") = forAll {
    (authCookies: AuthCookies) => {
      val expectedCookies = Set(
        RequestCookie("unifises", authCookies.uniFiSes),
        RequestCookie("csrf_token", authCookies.csrfToken)
      )

      Request[Id]().addAuthCookies(authCookies).cookies.toSet == expectedCookies
    }
  }
}
