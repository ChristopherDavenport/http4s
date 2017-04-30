package org.http4s
package client
package middleware

import scala.concurrent.duration._
import scala.math.{pow, min, random}
import org.http4s.Status._
import fs2.Task

import scala.Either
import scala.Right
import scala.Left

object Retry {


  private[this] val RetriableStatuses = Set(
    RequestTimeout,
    // TODO Leaving PayloadTooLarge out until we model Retry-After
    InternalServerError,
    ServiceUnavailable,
    BadGateway,
    GatewayTimeout
  )

  def apply(backoff: Int => Option[FiniteDuration])(client: Client): Client = {
    def prepareLoop(req: Request, attempts: Int): Task[DisposableResponse] = {
      client.open(req).attempt flatMap {
        // TODO fs2 port - Reimplement request isIdempotent in some form
        case Right(dr @ DisposableResponse(Response(status, _, _, _, _), _)) if RetriableStatuses(status) =>
          backoff(attempts) match {
            case Some(duration) =>
              dr.dispose.flatMap(_ => nextAttempt(req, attempts, duration))
            case None =>
              Task.now(dr)
          }
        case Right(dr) =>
          Task.now(dr)
        case Left(e) =>
          backoff(attempts) match {
            case Some(duration) =>
              nextAttempt(req, attempts, duration)
            case None =>
              // info instead of error(e), because e is not discarded
              Task.fail(e)
          }
      }
    }

    def nextAttempt(req: Request, attempts: Int, duration: FiniteDuration): Task[DisposableResponse] = {
        prepareLoop(req.copy(body = EmptyBody), attempts + 1)
    }
      // TODO honor Retry-After header
      // Task.async { (prepareLoop(req.copy(body = EmptyBody), attempts + 1)) }

    client.copy(open = Service.lift(prepareLoop(_, 1)))
  }
}


object RetryPolicy {

  def exponentialBackoff(maxWait: Duration, maxRetry: Int): Int => Option[FiniteDuration] = {
    val maxInMillis = maxWait.toMillis
    k => if (k > maxRetry) None else Some(expBackoff(k, maxInMillis))
  }

  private def expBackoff(k: Int, maxInMillis: Long): FiniteDuration = {
    val millis = (pow(2.0, k.toDouble) - 1.0) * 1000.0
    val interval = min(millis, maxInMillis.toDouble)
    FiniteDuration((random * interval).toLong, MILLISECONDS)
  }
}
