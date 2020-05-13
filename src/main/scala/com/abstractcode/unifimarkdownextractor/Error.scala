package com.abstractcode.unifimarkdownextractor

import java.nio.file.Path

import com.abstractcode.unifimarkdownextractor.Error._
import org.http4s.Status

sealed trait Error extends Throwable {
  override def getMessage: String = this match {
    case AuthenticationFailure(status) => s"Authentication failure calling UniFi controller. Received status ${status.code}"
    case InvalidAuthenticationResponse => "The response to the authentication request couldn't be parsed"
    case TokenUnauthorised => "The authentication token was not accepted by the UniFi controller. It may have expired."
    case UniFiError(status) => s"Something broke calling the UniFi controller. Received status ${status.code}"
    case InvalidResponse(error) => s"The response from the UniFi controller could not be parsed.\n${error.getMessage}"
    case DirectoryCreationFailure(path, true, false) => s"Tried to create directory $path but it exists and is not a directory."
    case DirectoryCreationFailure(path, exists, isDirectory) => s"Tried to create directory $path but failed (exists = $exists, isDirectory = $isDirectory)"
  }
}

object Error {

  case class AuthenticationFailure(status: Status) extends Error
  case object InvalidAuthenticationResponse extends Error
  case object TokenUnauthorised extends Error
  case class UniFiError(status: Status) extends Error
  case class InvalidResponse(error: Throwable) extends Error

  case class DirectoryCreationFailure(path: Path, exists: Boolean, isDirectory: Boolean) extends Error
}