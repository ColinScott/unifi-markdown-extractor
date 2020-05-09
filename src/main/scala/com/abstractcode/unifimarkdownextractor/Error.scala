package com.abstractcode.unifimarkdownextractor

import java.nio.file.Path

import org.http4s.Status

sealed trait Error extends Throwable

object Error {

  case class AuthenticationFailure(status: Status) extends Error
  case object InvalidAuthenticationResponse extends Error
  case object TokenUnauthorised extends Error
  case class UniFiError(status: Status) extends Error
  case object InvalidResponse extends Error

  case class DirectoryCreationFailure(path: Path, exists: Boolean, isDirectory: Boolean) extends Error
}