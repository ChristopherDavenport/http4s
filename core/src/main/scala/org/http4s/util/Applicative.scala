package org.http4s.util

/**
  * Created by davenpcm on 2/21/17.
  */
trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  def ap[A,B](f: F[A => B])(fa: F[A]): F[B]
}
