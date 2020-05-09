package com.abstractcode.unifimarkdownextractor

import com.abstractcode.unifimarkdownextractor.configuration.{ControllerConfiguration, Credentials}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.AuthCookies
import org.http4s.implicits._

object Fixture {
  val fixedAuthCookies: AuthCookies = AuthCookies("abc", "def")

  val fixedControllerConfiguration: ControllerConfiguration = ControllerConfiguration(
    uri"https://example.com/",
    Credentials("a@example.com", "test123")
  )
}
