package com.abstractcode

import cats.MonadError

package object unifimarkdownextractor {
  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]
}
