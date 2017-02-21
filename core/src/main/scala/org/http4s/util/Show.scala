package org.http4s.util

/**
  * Created by davenpcm on 2/21/17.
  */
trait Show[A] {
  def show(a: A): String

}

object Show{
  def apply[A](implicit evidence: Show[A]): Show[A] = evidence
}
