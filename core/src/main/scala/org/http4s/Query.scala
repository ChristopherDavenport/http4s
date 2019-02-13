package org.http4s

import org.http4s.Query._
import org.http4s.internal.parboiled2.CharPredicate
import org.http4s.parser.QueryParser
import org.http4s.util.{Renderable, UrlCodingUtils, Writer}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Collection representation of a query string
  *
  * It is a indexed sequence of key and maybe a value pairs which maps
  * precisely to a query string, modulo the identity of separators.
  *
  * When rendered, the resulting `String` will have the pairs separated
  * by '&' while the key is separated from the value with '='
  */
final class Query private (private val pairs: Vector[KeyValue])
    extends Renderable {

  def length: Int = pairs.length

  def slice(from: Int, until: Int): Query = new Query(pairs.slice(from, until))

  def +:(elem: Query): Query =
    new Query((elem.pairs ++ pairs))

  def :+(elem: Query): Query =
    new Query(pairs ++ elem.pairs)

  def isEmpty: Boolean = pairs.isEmpty

  def exists(f: KeyValue => Boolean): Boolean = pairs.exists(f)

  def filterNot(f: KeyValue => Boolean): Query = new Query(pairs.filterNot(f))

  def filter(f: KeyValue => Boolean): Query = new Query(pairs.filter(f))

  def nonEmpty: Boolean = pairs.nonEmpty

  def toVector: Vector[(String, Option[String])] = pairs

  /** Render the Query as a `String`.
    *
    * Pairs are separated by '&' and keys are separated from values by '='
    */
  override def render(writer: Writer): writer.type = {
    var first = true
    def encode(s: String) =
      UrlCodingUtils.urlEncode(s, spaceIsPlus = false, toSkip = NoEncode)
    pairs.foreach {
      case (n, None) =>
        if (!first) writer.append('&')
        else first = false
        writer.append(encode(n))

      case (n, Some(v)) =>
        if (!first) writer.append('&')
        else first = false
        writer
          .append(encode(n))
          .append("=")
          .append(encode(v))
    }
    writer
  }

  /** Map[String, String] representation of the [[Query]]
    *
    * If multiple values exist for a key, the first is returned. If
    * none exist, the empty `String` "" is returned.
    */
  def params: Map[String, String] = new ParamsView(multiParams)

  /** Map[String, Seq[String] ] representation of the [[Query]]
    *
    * Params are represented as a `Seq[String]` and may be empty.
    */
  lazy val multiParams: Map[String, Seq[String]] = {
    if (pairs.isEmpty) Map.empty[String, Seq[String]]
    else {
      val m = mutable.Map.empty[String, ListBuffer[String]]
      pairs.foreach {
        case (k, None) => m.getOrElseUpdate(k, new ListBuffer)
        case (k, Some(v)) => m.getOrElseUpdate(k, new ListBuffer) += v
      }

      m.toMap
    }
  }

  protected def replaceQuery(query: Query): Query = query

    /** alias for containsQueryParam */
    def ?[K: QueryParamKeyLike](name: K): Boolean =
    _containsQueryParam(QueryParamKeyLike[K].getKey(name))

  /** alias for setQueryParams */
  def =?[T: QueryParamEncoder](q: Map[String, Seq[T]]): Query =
    setQueryParams(q)

  /** alias for withQueryParam */
  def +?[T: QueryParam]: Query =
    _withQueryParam(QueryParam[T].key, Nil)

  /** alias for withQueryParam */
  def +*?[T: QueryParam: QueryParamEncoder](value: T): Query =
    _withQueryParam(QueryParam[T].key, QueryParamEncoder[T].encode(value) :: Nil)

  /** alias for withQueryParam */
  def +*?[T: QueryParam: QueryParamEncoder](values: Seq[T]): Query =
    _withQueryParam(QueryParam[T].key, values.map(QueryParamEncoder[T].encode))

  /** alias for withQueryParam */
  def +?[K: QueryParamKeyLike, T: QueryParamEncoder](name: K, value: T): Query =
    +?(name, value :: Nil)

  /** alias for withQueryParam */
  def +?[K: QueryParamKeyLike](name: K): Query =
    _withQueryParam(QueryParamKeyLike[K].getKey(name), Nil)

  /** alias for withQueryParam */
  def +?[K: QueryParamKeyLike, T: QueryParamEncoder](name: K, values: Seq[T]): Query =
    _withQueryParam(QueryParamKeyLike[K].getKey(name), values.map(QueryParamEncoder[T].encode))

  /*
  /** alias for withMaybeQueryParam */
  def +??[K: QueryParamKeyLike, T: QueryParamEncoder](name: K, value: Maybe[T]): Query =
    _withMaybeQueryParam(QueryParamKeyLike[K].getKey(name), value map QueryParamEncoder[T].encode)

  /** alias for withMaybeQueryParam */
  def +??[T: QueryParam : QueryParamEncoder](value: Maybe[T]): Query =
    _withMaybeQueryParam(QueryParam[T].key, value map QueryParamEncoder[T].encode)
   */

  /** alias for withOptionQueryParam */
  def +??[K: QueryParamKeyLike, T: QueryParamEncoder](name: K, value: Option[T]): Query =
    _withOptionQueryParam(QueryParamKeyLike[K].getKey(name), value.map(QueryParamEncoder[T].encode))

  /** alias for withOptionQueryParam */
  def +??[T: QueryParam: QueryParamEncoder](value: Option[T]): Query =
    _withOptionQueryParam(QueryParam[T].key, value.map(QueryParamEncoder[T].encode))

  /** alias for removeQueryParam */
  def -?[T](implicit key: QueryParam[T]): Query =
    _removeQueryParam(key.key)

  /** alias for removeQueryParam */
  def -?[K: QueryParamKeyLike](key: K): Query =
    _removeQueryParam(QueryParamKeyLike[K].getKey(key))

  /**
    * Checks if a specified parameter exists in the [[Query]]. A parameter
    * without a name can be checked with an empty string.
    */
  def containsQueryParam[T](implicit key: QueryParam[T]): Boolean =
    _containsQueryParam(key.key)

  def containsQueryParam[K: QueryParamKeyLike](key: K): Boolean =
    _containsQueryParam(QueryParamKeyLike[K].getKey(key))

  private def _containsQueryParam(name: QueryParameterKey): Boolean =
    if (this.isEmpty) false
    else this.exists { case (k, _) => k == name.value }

  /**
    * Creates maybe a new `Query` without the specified parameter in query.
    * If no parameter with the given `key` exists then `this` will be
    * returned.
    */
  def removeQueryParam[K: QueryParamKeyLike](key: K): Query =
    _removeQueryParam(QueryParamKeyLike[K].getKey(key))

  private def _removeQueryParam(name: QueryParameterKey): Query =
    if (this.isEmpty) Query()
    else {
      val newQuery = this.filterNot { case (n, _) => n == name.value }
      replaceQuery(newQuery)
    }

  /**
    * Creates maybe a new `Query` with the specified parameters. The entire
    * [[Query]] will be replaced with the given one.
    */
  def setQueryParams[K: QueryParamKeyLike, T: QueryParamEncoder](params: Map[K, Seq[T]]): Query = {
    val penc = QueryParamKeyLike[K]
    val venc = QueryParamEncoder[T]
    val b = Query.newBuilder
    params.foreach {
      case (k, Seq()) => b += ((penc.getKey(k).value, None))
      case (k, vs) => vs.foreach(v => b += ((penc.getKey(k).value, Some(venc.encode(v).value))))
    }
    replaceQuery(b.result())
  }

  /**
    * Creates a new `Query` with the specified parameter in the [[Query]].
    * If a parameter with the given `QueryParam.key` already exists the values will be
    * replaced with an empty list.
    */
  def withQueryParam[T: QueryParam]: Query =
    _withQueryParam(QueryParam[T].key, Nil)

  /**
    * Creates a new `Query` with the specified parameter in the [[Query]].
    * If a parameter with the given `key` already exists the values will be
    * replaced with an empty list.
    */
  def withQueryParam[K: QueryParamKeyLike](key: K): Query =
    _withQueryParam(QueryParamKeyLike[K].getKey(key), Nil)

  /**
    * Creates maybe a new `Query` with the specified parameter in the [[Query]].
    * If a parameter with the given `key` already exists the values will be
    * replaced. If the parameter to be added equal the existing entry the same
    * instance of `Query` will be returned.
    */
  def withQueryParam[T: QueryParamEncoder, K: QueryParamKeyLike](key: K, value: T): Query =
    _withQueryParam(QueryParamKeyLike[K].getKey(key), QueryParamEncoder[T].encode(value) :: Nil)

  /**
    * Creates maybe a new `Query` with the specified parameters in the [[Query]].
    * If a parameter with the given `key` already exists the values will be
    * replaced.
    */
  def withQueryParam[T: QueryParamEncoder, K: QueryParamKeyLike](key: K, values: Seq[T]): Query =
    _withQueryParam(QueryParamKeyLike[K].getKey(key), values.map(QueryParamEncoder[T].encode))

  private def _withQueryParam(name: QueryParameterKey, values: Seq[QueryParameterValue]): Query = {
    val b = Query.newBuilder
    toVector.foreach { case kv @ (k, _) => if (k != name.value) b += kv }
    if (values.isEmpty) b += ((name.value, None))
    else
      values.foreach { v =>
        b += ((name.value, Some(v.value)))
      }

    replaceQuery(b.result())
  }

  /*
  /**
   * Creates maybe a new `Query` with the specified parameter in the [[Query]].
   * If the value is empty the same instance of `Query` will be returned.
   * If a parameter with the given `key` already exists the values will be
   * replaced.
   */
  def withMaybeQueryParam[T: QueryParamEncoder, K: QueryParamKeyLike](key: K, value: Maybe[T]): Query =
    _withMaybeQueryParam(QueryParamKeyLike[K].getKey(key), value map QueryParamEncoder[T].encode)

  /**
   * Creates maybe a new `Query` with the specified parameter in the [[Query]].
   * If the value is empty or if the parameter to be added equal the existing
   * entry the same instance of `Query` will be returned.
   * If a parameter with the given `name` already exists the values will be
   * replaced.
   */
  def withMaybeQueryParam[T: QueryParam: QueryParamEncoder](value: Maybe[T]): Query =
    _withMaybeQueryParam(QueryParam[T].key, value map QueryParamEncoder[T].encode)
   */

  /**
    * Creates maybe a new `Query` with the specified parameter in the [[Query]].
    * If the value is empty or if the parameter to be added equal the existing
    * entry the same instance of `Query` will be returned.
    * If a parameter with the given `key` already exists the values will be
    * replaced.
    */
  def withOptionQueryParam[T: QueryParamEncoder, K: QueryParamKeyLike](
      key: K,
      value: Option[T]): Query =
    _withOptionQueryParam(QueryParamKeyLike[K].getKey(key), value.map(QueryParamEncoder[T].encode))

  /**
    * Creates maybe a new `Query` with the specified parameter in the [[Query]].
    * If the value is empty or if the parameter to be added equal the existing
    * entry the same instance of `Query` will be returned.
    * If a parameter with the given `name` already exists the values will be
    * replaced.
    */
  def withOptionQueryParam[T: QueryParam: QueryParamEncoder](value: Option[T]): Query =
    _withOptionQueryParam(QueryParam[T].key, value.map(QueryParamEncoder[T].encode))

  private def _withOptionQueryParam(
      name: QueryParameterKey,
      value: Option[QueryParameterValue]): Query =
    value.fold(this)(v => _withQueryParam(name, v :: Nil))
}

object Query {
  type KeyValue = (String, Option[String])

  type Builder = mutable.Builder[KeyValue, Query]

  val empty: Query = new Query(Vector.empty)

  /*
   * "The characters slash ("/") and question mark ("?") may represent data
   * within the query component... it is sometimes better for usability to
   * avoid percent-encoding those characters."
   *   -- http://tools.ietf.org/html/rfc3986#section-3.4
   */
  private val NoEncode: CharPredicate =
    UrlCodingUtils.Unreserved ++ "?/"

  def apply(xs: (String, Option[String])*): Query =
    new Query(xs.toVector)

  def fromPairs(xs: (String, String)*): Query = {
    val b = newBuilder
    xs.foreach { case (k, v) => b += ((k, Some(v))) }
    b.result()
  }

  /** Generate a [[Query]] from its `String` representation
    *
    * If parsing fails, the empty [[Query]] is returned
    */
  def fromString(query: String): Query =
    if (query.isEmpty) new Query(Vector("" -> None))
    else QueryParser.parseQueryString(query).right.toOption.getOrElse(Query.empty)

  /** Build a [[Query]] from the `Map` structure */
  def fromMap(map: Map[String, Seq[String]]): Query = {
    val b = newBuilder
    map.foreach {
      case (k, Seq()) => b += ((k, None))
      case (k, vs) => vs.foreach(v => b += ((k, Some(v))))
    }
    b.result()
  }

  def newBuilder: mutable.Builder[KeyValue, Query] =
    Vector.newBuilder[KeyValue].mapResult(v => new Query(v))
  ///////////////////////////////////////////////////////////////////////
  // Wrap the multiParams to get a Map[String, String] view
  private class ParamsView(wrapped: Map[String, Seq[String]]) extends Map[String, String] {
    override def +[B1 >: String](kv: (String, B1)): Map[String, B1] = {
      val m = wrapped + (kv)
      m.asInstanceOf[Map[String, B1]]
    }

    override def -(key: String): Map[String, String] = new ParamsView(wrapped - key)

    override def iterator: Iterator[(String, String)] =
      wrapped.iterator.map { case (k, s) => (k, s.headOption.getOrElse("")) }

    override def get(key: String): Option[String] =
      wrapped.get(key).flatMap(_.headOption)
  }
}
