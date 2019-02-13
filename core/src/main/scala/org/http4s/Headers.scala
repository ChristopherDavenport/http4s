package org.http4s

import cats._
import cats.implicits._
import org.http4s.headers.`Set-Cookie`
import org.http4s.syntax.string._
import org.http4s.util.CaseInsensitiveString
import scala.collection.mutable.ListBuffer

/** A collection of HTTP Headers */
final class Headers private (headers: List[Header]){

  def toList: List[Header] = headers

  def isEmpty: Boolean = headers.isEmpty

  def drop(n: Int): Headers = if (n == 0) this else new Headers(headers.drop(n))

  @deprecated("0.20.0-M6", "Use headOption")
  def head: Header = headers.head

  def headOption: Option[Header] = headers.headOption

  def nonEmpty: Boolean = headOption.isDefined

  def tail: Headers = headers match {
    case _ :: xs => Headers(xs)
    case Nil => Headers()
  }

  def collectFirst[A](f: PartialFunction[Header, A]): Option[A] = 
    headers.collectFirst(f)

  def foldLeft[A](a: A)(f: (A, Header)=> A): A =
    headers.foldLeft(a)(f)

  def filter(f: Header => Boolean): Headers = Headers(headers.filter(f))

  @deprecated("0.20.0-M6", "Never Use foreach")
  def foreach[B](f: Header => B): Unit = headers.foreach(f)

  /** Attempt to get a [[org.http4s.Header]] of type key.HeaderT from this collection
    *
    * @param key [[HeaderKey.Extractable]] that can identify the required header
    * @return a scala.Option possibly containing the resulting header of type key.HeaderT
    * @see [[Header]] object and get([[org.http4s.util.CaseInsensitiveString]])
    */
  def get(key: HeaderKey.Extractable): Option[key.HeaderT] = key.from(this)

  @deprecated(
    "Use response.cookies instead. Set-Cookie is unique among HTTP headers in that it can be repeated but can't be joined by a ','. This will return only the first Set-Cookie header. `response.cookies` will return the complete list.",
    "0.16.0-RC1"
  )
  def get(key: `Set-Cookie`.type): Option[`Set-Cookie`] =
    key.from(this).headOption

  /** Attempt to get a [[org.http4s.Header]] from this collection of headers
    *
    * @param key name of the header to find
    * @return a scala.Option possibly containing the resulting [[org.http4s.Header]]
    */
  def get(key: CaseInsensitiveString): Option[Header] = headers.find(_.name == key)

  /** Make a new collection adding the specified headers, replacing existing headers of singleton type
    * The passed headers are assumed to contain no duplicate Singleton headers.
    *
    * @param in multiple [[Header]] to append to the new collection
    * @return a new [[Headers]] containing the sum of the initial and input headers
    */
  def put(in: Header*): Headers =
    if (in.isEmpty) this
    else if (this.isEmpty) new Headers(in.toList)
    else this ++ Headers(in.toList)

  /** Concatenate the two collections
    * If the resulting collection is of Headers type, duplicate Singleton headers will be removed from
    * this Headers collection.
    *
    * @param that collection to append
    * @tparam B type contained in collection `that`
    * @tparam That resulting type of the new collection
    */
  def ++(that: Headers): Headers =
    if (that.isEmpty) this
    else if (this.isEmpty) that 
    else {
      val hs = that.toList
      val acc = new ListBuffer[Header]
      this.headers.foreach { orig =>
        orig.parsed match {
          case _: Header.Recurring => acc += orig
          case _: `Set-Cookie` => acc += orig
          case h if !hs.exists(_.name == h.name) => acc += orig
          case _ => // NOOP, drop non recurring header that already exists
        }
      }
      val h = new Headers(acc.prependToList(hs))
      h
    }

  override def hashCode(): Int = this.headers.hashCode()

  override def equals(that: Any): Boolean = that match {
    case otherheaders: Headers =>
      this.toList.equals(otherheaders.toList)
    case _ => false
  }

  /** Removes the `Content-Length`, `Content-Range`, `Trailer`, and
    * `Transfer-Encoding` headers.
    *
    *  https://tools.ietf.org/html/rfc7231#section-3.3
    */
  def removePayloadHeaders: Headers =
    Headers(toList.filterNot(h => Headers.PayloadHeaderKeys(h.name)))

  def redactSensitive(
      redactWhen: CaseInsensitiveString => Boolean = Headers.SensitiveHeaders.contains): Headers =
    Headers(
      headers.map {
      case h if redactWhen(h.name) => Header.Raw(h.name, "<REDACTED>")
      case h => h
      }
    )
}

object Headers {
  val empty = apply()

  /** Create a new Headers collection from the headers */
  def apply(headers: Header*): Headers = Headers(headers.toList)

  /** Create a new Headers collection from the headers */
  def apply(headers: List[Header]): Headers = new Headers(headers)


  implicit val headersShow: Show[Headers] =
    Show.show[Headers] {
      _.toList.map(_.show).mkString("Headers(", ", ", ")")
    }

  implicit val HeadersEq: Eq[Headers] = Eq.by(_.toList)

  private val PayloadHeaderKeys = Set(
    "Content-Length".ci,
    "Content-Range".ci,
    "Trailer".ci,
    "Transfer-Encoding".ci
  )

  val SensitiveHeaders = Set(
    "Authorization".ci,
    "Cookie".ci,
    "Set-Cookie".ci
  )
}
