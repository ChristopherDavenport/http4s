package org.http4s.util

/**
  * Created by davenpcm on 2/21/17.
  */
trait Eq[A] {
  def ==(x: A, y: A): Boolean
  def /=(x: A, y: A): Boolean = !==(x, y)
}

object Eq {
  def apply[A](implicit evidence: Eq[A]): Eq[A] = evidence
}
