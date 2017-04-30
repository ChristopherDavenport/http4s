package org.http4s
package client

import java.util.concurrent.ExecutorService
import scala.annotation.tailrec
import scala.collection.mutable
import fs2._
import cats.implicits._

private final class PoolManager[A <: Connection](builder: ConnectionBuilder[A],
                                                 maxTotal: Int,
                                                 es: ExecutorService)
  extends ConnectionManager[A] {

  private sealed case class Waiting(key: RequestKey, callback: Callback[NextConnection])

  implicit val strategy : Strategy = Strategy.fromExecutor(es)

  private var isClosed = false
  private var allocated = 0
  private val idleQueue = new mutable.Queue[A]
  private val waitQueue = new mutable.Queue[Waiting]

  private def stats =
    s"allocated=${allocated} idleQueue.size=${idleQueue.size} waitQueue.size=${waitQueue.size}"

  /**
    * This method is the core method for creating a connection which increments allocated synchronously
    * then builds the connection with the given callback and completes the callback.
    *
    * If we can create a connection then it initially increments the allocated value within a region
    * that is called synchronously by the calling method. Then it proceeds to attempt to create the connection
    * and feed it the callback. If we cannot create a connection because we are already full then this
    * completes the callback on the error synchronously.
    *
    * @param key The RequestKey for the Connection.
    * @param callback The callback to complete with the NextConnection.
    */
  private def createConnection(key: RequestKey, callback: Callback[NextConnection]): Unit = {
    if (allocated < maxTotal){
      allocated += 1
      builder(key).unsafeRunAsync {
        case Right(conn) => callback(Right(NextConnection(conn, true)))
        case Left(error) =>
          disposeConnection(key, None)
          callback(Left(error))
      }
    }
    else {
      val message = s"Invariant broken in ${this.getClass.getSimpleName}! Tried to create more connections than allowed: ${stats}"
      val error = new Exception(message)
      callback(Left(error))
    }
  }

  /**
    * This generates a Task of Next Connection. The following calls are executed asynchronously
    * with respect to whenever the execution of this task can occur.
    *
    * If the pool is closed The task failure is executed.
    *
    * If the pool is not closed then we look for any connections in the idleQueue that match
    * the RequestKey requested.
    * If a matching connection exists and it is stil open the callback is executed with the connection.
    * If a matching connection is closed we deallocate and repeat the check through the idleQueue.
    * If no matching connection is found, and the pool is not full we create a new Connection to perform
    * the request.
    * If no matching connection is found and the pool is full, and we have connections in the idleQueue
    * then a connection in the idleQueue is shutdown and a new connection is created to perform the request.
    * If no matching connection is found and the pool is full, and all connections are currently in use
    * then the Request is placed in a waitingQueue to be executed when a connection is released.
    *
    * @param key The Request Key For The Connection
    * @return A Task of NextConnection
    */
   def borrow(key: RequestKey): Task[NextConnection] = Task.async{ callback =>
     synchronized {
       if (!isClosed) {
         @tailrec
         def go(): Unit = {
           idleQueue.dequeueFirst(_.requestKey == key) match {
             case Some(conn) if !conn.isClosed =>
               callback(Right(NextConnection(conn, false)))

             case Some(closedConn) =>
               allocated -= 1
               go()

             case None if allocated < maxTotal =>
               createConnection(key, callback)

             case None if idleQueue.nonEmpty =>
               allocated -= 1
               idleQueue.dequeue().shutdown()
               createConnection(key, callback)

             case None => // we're full up. Add to waiting queue.
               waitQueue.enqueue(Waiting(key, callback))
           }
         }
         go()
       }
       else {
         callback(Left(new IllegalStateException("Connection pool is closed")))
       }
     }
   }

  /**
    * This is how connections are returned to the ConnectionPool.
    *
    * If the pool is closed the connection is shutdown and logged.
    * If it is not closed we check if the connection is recyclable.
    *
    * If the connection is Recyclable we check if any of the connections in the waitQueue
    * are looking for the returned connections RequestKey.
    * If one is the first found is given the connection.And runs it using its callback asynchronously.
    * If one is not found and the waitingQueue is Empty then we place the connection on the idle queue.
    * If the waiting queue is not empty and we did not find a match then we shutdown the connection
    * and create a connection for the first item in the waitQueue.
    *
    * If it is not recyclable, and it is not shutdown we shutdown the connection. If there
    * are values in the waitQueue we create a connection and execute the callback asynchronously.
    * Otherwise the pool is shrunk.
    *
    * @param connection The connection to be released.
    * @return A Task of Unit
    */
  def release(connection: A): Task[Unit] = Task.delay {
    synchronized {
      if (!isClosed) {
        val key = connection.requestKey
        if (connection.isRecyclable) {
          waitQueue.dequeueFirst(_.key == key) match {
            case Some(Waiting(_, callback)) =>
              callback(Right(NextConnection(connection, false)))

            case None if waitQueue.isEmpty =>
              idleQueue.enqueue(connection)

            // returned connection didn't match any pending request: kill it and start a new one for a queued request
            case None =>
              connection.shutdown()
              allocated -= 1
              val Waiting(key, callback) = waitQueue.dequeue()
              createConnection(key, callback)
          }
        }
        else {
          allocated -= 1

          if (!connection.isClosed) {
            connection.shutdown()
          }

          if (waitQueue.nonEmpty) {
            val Waiting(key, callback) = waitQueue.dequeue()
            createConnection(key, callback)
          }
        }
      }
      else if (!connection.isClosed) {
        connection.shutdown()
        allocated -= 1
      }
    }
  }

  /**
    * This invalidates a Connection. This is what is exposed externally, and
    * is just a Task wrapper around disposing the connection.
    *
    * @param connection The connection to invalidate
    * @return A Task of Unit
    */
  override def invalidate(connection: A): Task[Unit] =
    Task.delay(disposeConnection(connection.requestKey, Some(connection)))

  /**
    * Synchronous Immediate Disposal of a Connection and Its Resources.
    *
    * By taking an Option of a connection this also serves as a synchronized allocated decrease.
    *
    * @param key The request key for the connection. Not used internally.
    * @param connection An Option of a Connection to Dispose Of.
    */
  private def disposeConnection(key: RequestKey, connection: Option[A]): Unit = {
    synchronized {
      allocated -= 1
      connection.foreach { s => if (!s.isClosed) s.shutdown() }
    }
  }

  /**
    * Shuts down the connection pool permanently.
    *
    * Changes isClosed to true, no methods can reopen a closed Pool.
    * Shutdowns all connections in the IdleQueue and Sets Allocated to Zero
    *
    * @return A Task Of Unit
    */
  def shutdown() : Task[Unit] = Task.delay {
    synchronized {
      if (!isClosed) {
        isClosed = true
        idleQueue.foreach(_.shutdown())
        allocated = 0
      }
    }
  }
}
