package com.abstractcode.unifimarkdownextractor

import org.http4s.Status

sealed trait Error extends Throwable

case class AuthenticationFailure(status: Status) extends Error
case object InvalidAuthenticationResponse extends Error
