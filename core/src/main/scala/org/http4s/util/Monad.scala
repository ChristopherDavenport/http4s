package org.http4s.util

/**
  * Created by davenpcm on 2/21/17.
  */
trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A,B](a: F[A])(f: A => F[B]): F[B]

  def map[A,B](a: F[A])(f: A => B): F[B] = flatMap(a)(f andThen pure)
  def ap[A,B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))
}

object Monad {
  def apply[F[_]](implicit evidence: Monad[F]): Monad[F] = evidence
}
