package org.http4s

trait QueryOps {

  protected type Self <: QueryOps

  protected val query: Query

  protected def self: Self

  protected def replaceQuery(query: Query): Self

}
