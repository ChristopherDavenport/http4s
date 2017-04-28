package org.http4s
package multipart

import java.nio.charset.StandardCharsets

import fs2._
import fs2.util.syntax._
import cats.implicits._
import fs2.util.{Catchable, Effect}
import org.http4s.multipart
import scodec.bits.ByteVector

/** A low-level multipart-parsing pipe.  Most end users will prefer EntityDecoder[Multipart]. */
object MultipartParser {

  def untilMatching[F[_], A](find: Seq[A])(stream: Stream[F, A]): (Stream[F, A], Stream[F, A], Stream[F, A]) = {
    val matchValue = find.toVector
    val slidingWindow = stream.through(pipe.sliding(matchValue.length))
    val emitHeads: Pipe[F, Vector[A], A] = _.flatMap(_.headOption.map(Stream.emit).getOrElse(Stream.empty))

    val outputBefore = slidingWindow.takeWhile(_ != matchValue).through(emitHeads)
    val outputMatch = slidingWindow.find(_ != matchValue).flatMap(Stream.emits)
    val outputAfter = slidingWindow.dropWhile(_ != matchValue).through(emitHeads)

    (outputBefore, outputMatch, outputAfter)
  }

  private[this] val logger = org.log4s.getLogger

  private val CRLF = ByteVector('\r', '\n')
  private val DASHDASH = ByteVector('-', '-')

  def parse(boundary: Boundary): Pipe[Task,Byte, Either[Headers, Byte]] = initStream => {
    val delimiter : Seq[Byte] = DASHDASH.toSeq ++ boundary.value.getBytes(StandardCharsets.UTF_8)
    val endDelimiter = delimiter ++ DASHDASH.toSeq

    val (preamble, delimiterR, rest) = untilMatching(delimiter)(initStream)
    val (parts, closeDelimiterR, epilogue) = untilMatching(endDelimiter)(rest)

    parts.through(parseParts(delimiter)).through(splitPart)
  }

  // Split Completed Parts
  def splitPart: Pipe[Task, Part, Either[Headers, Byte]] = _.flatMap{
    case Part(headers, body) => Stream.emit(Left(headers)) ++ body.map(Either.right)
  }

  def parseParts(delimiter: Seq[Byte]): Pipe[Task, Byte, Part] = initStream => {
    val (part, _, tail) = untilMatching(delimiter)(initStream)
    part.through(parsePart) ++ tail.through(parseParts(delimiter))
  }

  def parsePart: Pipe[Task, Byte, Part] = initStream => {
    val (headers, _, body) = untilMatching(CRLF.toSeq ++ CRLF.toSeq)(initStream)

    Stream.eval(parseHeaders(headers)).map(Part(_, body))
  }

  def parseHeaders(initStream: Stream[Task, Byte]): Task[Headers] = {
    initStream.through(text.utf8Decode)
      .through(text.lines)
      .through(parseHeader)
      .runFold(Headers.empty){
        case (headers, newHeader) => headers.put(newHeader)
      }
  }

  def parseHeader: Pipe[Task, String, Header] = {
    def getHeaders[F[_]](h: Handle[F, String]): Pull[F, Header, Handle[F, String]] = {
      h.await1.optional.flatMap {
        case (Some((line, h))) =>
            parseHeaderString(line) match {
              case Some(header) => Pull.output1(header) >> getHeaders(h)
              case None => getHeaders(h)
            }
        case None => Pull.done
      }
    }
    def parseHeaderString(string: String): Option[Header] = {
      val idx = string.indexOf(":")
      if (idx > 0 && idx < string.length - 1) Some(Header(string.substring(0, idx), string.substring(idx + 1).trim))
      else {
        logger.debug(s"Failed to parse Header String: $string")
        None
      }
    }

    _.pull(getHeaders).covary[Task]
  }

}

//
//def parse[F[_]](boundary: Boundary)(implicit e: Effect[F]): Pipe[Task,Byte, Either[Headers, Byte]] = initStream => {
//  val boundaryBytes = boundary.toChunk
//  val delimiter : Seq[Byte] = DASHDASH.toSeq ++ boundary.value.getBytes(StandardCharsets.UTF_8)
//  val endDelimiter = delimiter ++ DASHDASH.toSeq
//
//  val (preamble, delimiterR, rest) = untilMatching(delimiter)(initStream)
//  val (parts, closeDelimiterR, epilogue) = untilMatching(endDelimiter)(rest)
//
//  parts.through(parseParts(delimiter)).through(splitPart)
//}
//
//  // Must Use Task As Body Is Still Hardcoded to Task
//  def splitPart: Pipe[Task, Part, Either[Headers, Byte]] = _.flatMap{
//  case Part(headers, body) => Stream.emit(Left(headers)) ++ body.map(Either.right)
//}
//
//  def parseParts[F[_]](delimiter: Seq[Byte])(implicit e: Effect[F]): Pipe[Task, Byte, Part] = initStream => {
//  val (part, _, tail) = untilMatching(delimiter)(initStream)
//  part.through(parsePart) ++ tail.through(parseParts(delimiter))
//}
//
//  def parsePart[F[_]]: Pipe[Task, Byte, Part] = initStream => {
//  val (headers, _, body) = untilMatching(CRLF.toSeq ++ CRLF.toSeq)(initStream)
//
//  Stream.eval(parseHeaders(headers)).map(Part(_, body))
//}
//
//  def parseHeaders[F[_]](initStream: Stream[Task, Byte])(implicit e: Effect[F]): F[Headers] = {
//  initStream.through(text.utf8Decode)
//  .through(text.lines)
//  .through(parseHeader)
//  .runFold(Headers.empty){
//  case (headers, newHeader) => headers.put(newHeader)
//}
//}
//
//  def parseHeader[F[_]](implicit e: Effect[F]): Pipe[F, String, Header] = _.flatMap{ line =>
//  val idx = line.indexOf(":")
//  val headerOpt =
//  if (idx > 0 && idx < line.length - 1) Some(Header(line.substring(0, idx), line.substring(idx + 1).trim))
//  else None
//  headerOpt.map(Stream.emit).getOrElse(Stream.fail(new Throwable(s"Failed To Parse Header in Line $line")))
//}