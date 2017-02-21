package org.http4s.util

/**
  * Created by davenpcm on 2/21/17.
  */
final case class Kleisli[F[_], A, B](run: A => F[B])
