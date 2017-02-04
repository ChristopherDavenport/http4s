package org.http4s
package client

import java.util.concurrent.ExecutorService
import org.log4s.getLogger
import scala.annotation.tailrec
import fs2._
import cats.implicits._
import fs2.async.mutable.Queue

private final class PoolManager[A <: Connection](builder: ConnectionBuilder[A],
                                                 maxTotal: Int
                                                 )
  extends ConnectionManager[A] {

  private sealed case class Waiting(key: RequestKey, callback: Callback[NextConnection])

  private[this] val logger = getLogger
  private var isClosed = false
  private var allocated = 0
  private val idleQueue = Queue.bounded[Task,NextConnection](maxTotal)
  private val waitQueue = Queue.unbounded[Task, Waiting]

  private def stats : Task[String] = {
    for {
      iQueue <- idleQueue
      wQueue <- waitQueue
    } yield {
      s"allocated=${allocated} idleQueue.size=${iQueue.size.get} waitQueue.size=${wQueue.size.get}"
    }
  }


  private def createConnection(key: RequestKey): Task[NextConnection] = {
    if (allocated < maxTotal){
      builder(key).map(NextConnection(_, true))
    } else {
      stats.flatMap { errorStats =>
        val message = s"Invariant broken in ${this.getClass.getSimpleName}! Tried to create more connections than allowed: ${errorStats}"
        val error = new Exception(message)
        logger.error(error)(message)
        Task.fail(error)
      }
    }
  }


  // TODO fs2 rework
  // def borrow(key: RequestKey)(callback: Callback[NextConnection]): Task[NextConnection] = Task.delay{
  //   logger.debug(s"Requesting connection: ${stats}")
  //   synchronized {
  //     if (!isClosed) {
  //       @tailrec
  //       def go(): Task[NextConnection] = {
  //         idleQueue.dequeueFirst(_.requestKey == key) match {
  //           case Some(conn) if !conn.isClosed =>
  //             logger.debug(s"Recycling connection: ${stats}")
  //             callback(NextConnection(conn, false).right.toEither)
  //
  //           case Some(closedConn) =>
  //             logger.debug(s"Evicting closed connection: ${stats}")
  //             allocated -= 1
  //             go()
  //
  //           case None if allocated < maxTotal =>
  //             logger.debug(s"Active connection not found. Creating new one. ${stats}")
  //             createConnection(key, callback)
  //
  //           case None if idleQueue.nonEmpty =>
  //             logger.debug(s"No connections available for the desired key. Evicting oldest and creating a new connection: ${stats}")
  //             allocated -= 1
  //             idleQueue.dequeue().shutdown()
  //             createConnection(key, callback)
  //
  //           case None => // we're full up. Add to waiting queue.
  //             logger.debug(s"No connections available.  Waiting on new connection: ${stats}")
  //             waitQueue.enqueue(Waiting(key, callback))
  //         }
  //       }
  //       go()
  //     }
  //     else
  //       callback(new IllegalStateException("Connection pool is closed").left.toEither)
  //   }
  // }

  def release(connection: A): Task[Unit] = {



    synchronized {

      if (!isClosed) {
        logger.debug(s"Recycling connection: ${stats}")

        for {
          key <- connection.requestKey
          recyclable <- connection.isRecyclable
          wQueue <- waitQueue
        } yield {
          if (recyclable) {
            wQueue.dequeue1.map { waiting =>
              if (waiting.key == key) {


                case Some(Waiting(_, callback)) =>
                  logger.debug(s"Fulfilling waiting connection request: ${stats}")
                  callback(Right(NextConnection(connection, false)))

                wQueue

                case None if wQueue.size.get =>
                  logger.debug(s"Returning idle connection to pool: ${stats}")
                  idleQueue.enqueue(connection)

                // returned connection didn't match any pending request: kill it and start a new one for a queued request
                case None =>
                  connection.shutdown()
                  allocated -= 1
                  val Waiting(key, callback) = waitQueue.map(_.dequeue1)
                  createConnection(key)
              }
            }
          }
          else {

          }
        }
      } else {
          allocated -= 1

          if (!connection.isClosed) {
            logger.debug(s"Connection returned was busy.  Shutting down: ${stats}")
            connection.shutdown()
          }

          if (waitQueue.nonEmpty) {
            logger.debug(s"Connection returned could not be recycled, new connection needed: ${stats}")
            val Waiting(key, callback) = waitQueue.dequeue()
            createConnection(key, callback)
          }
          else logger.debug(s"Connection could not be recycled, no pending requests. Shrinking pool: ${stats}")
        }
      }
      else if (!connection.isClosed) {
        logger.debug(s"Shutting down connection after pool closure: ${stats}")
        connection.shutdown()
        allocated -= 1
      }
    }
  }

  override def invalidate(connection: A): Task[Unit] = connection.requestKey.flatMap( key =>
    disposeConnection(key, Some(connection))
  )


  private def disposeConnection(key: RequestKey, connection: Option[A]) : Task[Unit] = {
    logger.debug(s"Disposing of connection: ${stats}")
    synchronized {
      allocated -= 1
      connection.foreach { s => if (!s.isClosed) s.shutdown() }
    }
  }

  def shutdown(): Task[Unit] =
    stats.flatMap{ shutdownStats =>
      logger.info(s"Shutting down connection pool: ${shutdownStats}")
      synchronized {
        if (!isClosed) {
          isClosed = true
          idleQueue.flatMap{iQueue =>
            iQueue.dequeueAvailable.fold(Task[Unit](()))((unitTask, nextConn) =>
              unitTask.flatMap(_ => nextConn.connection.shutdown())
            ).run
          }
        }
        else {
          Task.now[Unit](())
        }
      }
  }
}
