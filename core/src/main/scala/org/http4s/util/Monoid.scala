package org.http4s.util

/**
  * Created by davenpcm on 2/21/17.
  */
trait Monoid[A] extends Semigroup[A] {
  def mempty: A
}

object Monoid {
  def apply[A](implicit evidence: Monoid[A]): Monoid[A] = evidence
}