package com.abstractcode.unifimarkdownextractor

import com.abstractcode.unifimarkdownextractor.configuration.{AppConfiguration, Credentials}
import com.abstractcode.unifimarkdownextractor.unifiapi.models.AuthCookies
import org.http4s.implicits._

object Fixture {
  val fixedAuthCookies: AuthCookies = AuthCookies("abc", "def")

  val fixedAppConfiguration: AppConfiguration = AppConfiguration(
    uri"https://example.com/",
    Credentials("a@example.com", "test123")
  )
}
