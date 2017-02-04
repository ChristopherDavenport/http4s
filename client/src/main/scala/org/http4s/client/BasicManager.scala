package org.http4s
package client

import fs2.Task

private final class BasicManager[A <: Connection](builder: ConnectionBuilder[A]) extends ConnectionManager[A] {

  /**
    * Get a connection for the provided request key. It does so by running
    * the ConnectionBuilder by applying a RequestKey to get a Task[A] which as
    * This is a fresh connection from the builder so it is always true and
    * a NextConnection is returned within the task.
    * @param requestKey Represents a key for requests that can conceivably share a [[Connection]]
    *                   although in this implementation it never will.
    * @return A Task Of NextConnection to be
    */
  def borrow(requestKey: RequestKey): Task[NextConnection] = {
    builder(requestKey).map { connection =>
      val isFresh = true
      NextConnection(connection, isFresh)
    }
  }

  override def shutdown(): Task[Unit] =
    Task.now(())

  override def invalidate(connection: A): Task[Unit] =
    Task.delay(connection.shutdown())

  override def release(connection: A): Task[Unit] =
    invalidate(connection)
}
