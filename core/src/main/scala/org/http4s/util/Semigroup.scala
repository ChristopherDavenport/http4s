package org.http4s.util

/**
  * Created by davenpcm on 2/21/17.
  */
trait Semigroup[A] {
  def mappend(x: A, y: A): A
}

object Semigroup {
  def apply[A](implicit evidence: Semigroup[A]): Semigroup[A] = evidence
}
