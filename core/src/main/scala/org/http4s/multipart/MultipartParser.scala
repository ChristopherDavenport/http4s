package org.http4s
package multipart

import scodec.bits.ByteVector
import fs2._
import fs2.Stream._
import cats.syntax.either._

/** A low-level multipart-parsing pipe.  Most end users will prefer EntityDecoder[Multipart]. */
object MultipartParser {

  private[this] val logger = org.log4s.getLogger

  private val CRLF = ByteVector('\r', '\n')
  private val DASHDASH = ByteVector('-', '-')

  private final case class Out[+A](a: A, tail: Option[ByteVector] = None)

  def parse[F[_]](boundary: Boundary): Pipe[F, ByteVector, Either[Headers,ByteVector]] = {
    val boundaryBytes = boundary.toByteVector
    val startLine = DASHDASH ++ boundaryBytes
    val endLine = startLine ++ DASHDASH

    // At various points, we'll read up until we find this, to loop back into beginPart.
    val expected = CRLF ++ startLine

    // TODO may need a custom Pull for individual lines due to CRLF across chunks
    def receiveLine: Pipe[F, ByteVector, Either[ByteVector, Out[ByteVector]]] = s => {

      def handle: Pipe[F, ByteVector, Either[ByteVector, Out[ByteVector]]] = s => s.flatMap{ bv =>
        logger.trace(s"handle: $bv")
        val index = bv indexOfSlice CRLF

        if (index >= 0) {
          val (head, tailPre) = bv splitAt index
          val tail = tailPre drop 2       // remove the line feed characters
          val tailM = Some(tail) filter { !_.isEmpty }
          Stream.emit(Either.right(Out(head, tailM)))
        }
        else if (bv.nonEmpty && bv.get(bv.length - 1) == '\r') {
          // The CRLF may have split across chunks.
          val (head, cr) = bv.splitAt(bv.length - 1)
          Stream.emit(Either.left(head)) //++ receive1(next => handle(cr ++ next))
        }
        else {
          Stream.emit(Either.left(bv)) //++ receiveLine(None)
        }
      }

      s.through(handle)
//      leading map handle getOrElse receive1(handle)
    }

    def receiveCollapsedLine: Pipe[F, ByteVector, Out[ByteVector]] = s => {
      s
        .through(receiveLine)
        .through{pipe.fold(Out(ByteVector.empty)) {
          case (acc, Left(partial)) => acc.copy(acc.a ++ partial)
          case (acc, Right(Out(term, tail))) => Out(acc.a ++ term, tail)
        }}
    }

    def start: Pipe[F, ByteVector, Either[Headers, ByteVector]] = {
      def beginPart: Pipe[F, ByteVector, Option[ByteVector]] = s => {
        def isStartLine(line: ByteVector): Boolean =
          line.startsWith(startLine) && isTransportPadding(line.drop(startLine.size))

        def isEndLine(line: ByteVector): Boolean =
          line.startsWith(endLine) && isTransportPadding(line.drop(endLine.size))

        def isTransportPadding(bv: ByteVector): Boolean =
          bv.toSeq.find(b => b != ' ' && b != '\t').isEmpty

        s.through(receiveCollapsedLine) flatMap  { case Out(line, tail) =>
          if (isStartLine(line)) {
            logger.debug("Found start line. Beginning new part.")
            emit(tail)
          }
          else if (isEndLine(line)) {
            logger.debug("Found end line. Halting.")
            Stream.empty
          }
          else {
            logger.trace(s"Discarding prelude: $line")
            tail.map(Stream.emit(_).through(beginPart)).getOrElse(Stream.empty)
          }
        }
      }

      def go: Pipe[F, ByteVector, Either[Headers,ByteVector]] = s => {
        for {
          tail <- s.through(beginPart)
          headerPair <- header(tail, expected)
          Out(headers, tail2) = headerPair
          _ = logger.debug(s"Headers: $headers")
          spacePair <- tail2.map(Stream.emit).getOrElse(Stream.empty).through(receiveCollapsedLine)
          tail3 = spacePair.tail // eat the space between header and content
          part <- emit(Either.left(headers)) ++ body(tail3.map(_.compact), expected) {
            case Out(chunk, None) =>
              logger.debug(s"Chunk: $chunk")
              emit(Either.right(chunk))
            case Out(ByteVector.empty, tail) =>
              logger.trace(s"Resuming with $tail.")
              tail.map(Stream.emit).getOrElse(Stream.empty).through(go)
            case Out(chunk, tail) =>
              logger.debug(s"Last chunk: $chunk.")
              logger.trace(s"Resuming with $tail.")
              emit(Either.right(chunk)) ++ tail.map(Stream.emit).getOrElse(Stream.empty).through(go)
          }
        } yield part
      }


    def header(leading: Option[ByteVector], expected: ByteVector): Pipe[F, ByteVector, Out[Headers]] = {
      def go(leading: Option[ByteVector], expected: ByteVector): Pipe[F, ByteVector, Out[Headers]] = {
        receiveCollapsedLine(leading) flatMap {
          case Out(bv, tail) => {
            if (bv == expected) {
              Stream.empty
            } else {
              val headerM = for {
                line <- bv.decodeAscii.right.toOption
                idx <- Some(line indexOf ':')
                if idx >= 0
                if idx < line.length - 1
              } yield Header(line.substring(0, idx), line.substring(idx + 1).trim)

              headerM.map { header => emit(Out(Headers(header), tail)) }
                .map { _ ++ go(tail, expected) }
                .getOrElse(halt)
            }
          }
        }
      }

      go(leading, expected).fold(Out(Headers.empty)) {
        case (acc, Out(header, tail)) => Out(acc.a ++ header, tail)
      }
    }

    def body(leading: Option[ByteVector], expected: ByteVector): Pipe[F, ByteVector, Out[ByteVector]] = {
      val heads = (0L until expected.length).scanLeft(expected) { (acc, _) =>
        acc dropRight 1
      }

      def pre: Pipe[F, ByteVector, Out[ByteVector]] = s => s.flatMap{ bv =>
        logger.trace(s"pre: $bv")
        val idx = bv indexOfSlice expected
        if (idx >= 0) {
          // if we find the terminator *within* the chunk, we need to just trip and be done
          // the +2 consumes the CRLF before the multipart boundary
          emit(Out(bv take idx, Some(bv drop (idx + 2))))
        } else {
          // find goes from front to back, and heads is in order from longest to shortest, thus we always greedily match
//          heads find (bv endsWith) map { tail =>
////            val current = emit(Out(bv.dropRight(tail.length)))
////            val midStream = Stream.emit(tail).through2(Stream.emit(expected.drop(tail.lenght))(mid)expected.drop(tail.length))
//            Stream.empty[Out[ByteVector]]
//          }.getOrElse(emit(Out(bv)))
//          heads find (bv endsWith) map { tail =>
//            emit(Out(bv dropRight tail.length)) ++ mid(tail, expected drop tail.length)
//          } getOrElse (emit(Out(bv))
          Stream.empty
        }
      }

      /* We might be looking at a boundary, or we might not.  This is how we
       * decide that incrementally.
       *
       * @param found the part of the next boundary that we've matched so far
       * @param remainder the part of the next boundary we're looking for on the next read.
       */
      def mid: Pipe2[F, ByteVector, ByteVector, Out[ByteVector]] = (f, r) => (f.zip(r)).flatMap{ case (found, remainder) =>
        logger.trace(s"mid: $found remainder")
        if (remainder.isEmpty) {
          // found should start with a CRLF, because mid is called with it.
          emit(Out(ByteVector.empty, Some(found.drop(2)))).covary[F]
        } else {
          receive1Or[ByteVector, Out[ByteVector]](
            fail(new MalformedMessageBodyFailure("Part was not terminated"))) { bv =>
            val (remFront, remBack) = remainder splitAt bv.length
            if (bv startsWith remFront) {
              // If remBack is nonEmpty, then the progress toward our match
              // is represented by bv, and we'll loop back to read more to
              // look for remBack.
              //
              // If remBack is empty, then `found ++ bv` starts with the
              // next boundary, and we'll emit out everything we've read
              // on the next loop.
              mid(found ++ bv, remBack)
            }
            else {
              // ok, so this buffer frame-slipped, but we might have a different terminator within bv
              emit(Out(found)) ++ pre(bv)
            }
          }
        }
      }

      leading map pre getOrElse receive1(pre)
    }

    start
  }
}

