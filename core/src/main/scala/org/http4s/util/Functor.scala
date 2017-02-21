package org.http4s
package util

/**
  * Created by davenpcm on 2/21/17.
  */
trait Functor[F[_]] {
  def fmap[A,B](f: A => B)(fa: F[A]): F[B]
}

object Functor {
  def apply[F[_]](implicit evidence: Functor[F]): Functor[F] = evidence
}
