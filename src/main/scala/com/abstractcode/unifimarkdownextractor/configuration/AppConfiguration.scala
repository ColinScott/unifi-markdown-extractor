package com.abstractcode.unifimarkdownextractor.configuration

import cats.data.ValidatedNel
import cats.implicits._
import com.abstractcode.unifimarkdownextractor.configuration.ParseError._
import org.http4s.Uri

case class AppConfiguration(
  controller: ControllerConfiguration
)

case class ControllerConfiguration(
  serverUri: Uri,
  credentials: Credentials
)

case class Credentials(username: String, password: String)

case class ParseError(environmentVariable: String, reason: Reason)

object ParseError {
  sealed trait Reason
  case object NotProvidedOrEmpty extends Reason
  case object InvalidFormat extends Reason
}

object AppConfiguration {
  def apply(env: Map[String, String]): ValidatedNel[ParseError, AppConfiguration] =
    getControllerConfiguration(env).map(AppConfiguration.apply)

  def getControllerConfiguration(env: Map[String, String]): ValidatedNel[ParseError, ControllerConfiguration] = (
    getUri(env, "SERVER_URI"),
    getCredentials(env)
    ).mapN(ControllerConfiguration.apply)

  def getUri(env: Map[String, String], key: String): ValidatedNel[ParseError, Uri] =
    env.get(key)
      .map(e => Uri.fromString(e).leftMap(_ => ParseError(key, InvalidFormat)).toValidatedNel)
      .getOrElse(ParseError(key, NotProvidedOrEmpty).invalidNel)

  def getNonEmptyString(env: Map[String, String], key: String): ValidatedNel[ParseError, String] = env.get(key)
    .filter(!_.trim.isEmpty)
    .toValidNel(ParseError(key, NotProvidedOrEmpty))

  def getCredentials(env: Map[String, String]): ValidatedNel[ParseError, Credentials] = (
    getNonEmptyString(env, "USERNAME"),
    getNonEmptyString(env, "PASSWORD")
    ).mapN(Credentials)

}