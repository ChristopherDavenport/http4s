package org.http4s
package multipart

import scodec.bits.ByteVector
import cats.syntax.either._
import fs2._
import fs2.Stream._



/** A low-level multipart-parsing pipe.  Most end users will prefer EntityDecoder[Multipart]. */
object MultipartParser {

  private[this] val logger = org.log4s.getLogger

  private val CRLF = ByteVector('\r', '\n')
  private val DASHDASH = ByteVector('-', '-')

  private[multipart] final case class Out[+A](a: A, tail: Option[ByteVector] = None)

  private[multipart] val toByteVector : Pipe[Task, Byte, ByteVector] =
    _.mapChunks(chunk => Chunk.singleton(ByteVector(chunk.toArray)))
  private[multipart] val toBytes : Pipe[Task, ByteVector, Byte] = _.flatMap(bv => Stream.emits(bv.toSeq))
  private[multipart] def eitherByteVectorToBytes[F[_], A]: Pipe[F, Either[A, ByteVector], Either[A, Byte]] =
    _.flatMap{
      case Right(bv) => Stream.emits(bv.toSeq.map(Either.right))
      case Left(a) => Stream.emit(Either.left(a))
    }

  private[multipart] def lines[F[_]] : Pipe[F, ByteVector, ByteVector] = {

    def linesFromByteVector(byteVector: ByteVector): (Vector[ByteVector], ByteVector) = {
      var i = 0L
      var start = 0L
      var out = Vector.empty[ByteVector]
      while (i < byteVector.size) {
        byteVector(i) match {
          case '\n' =>
            out = out :+ byteVector.slice(start, i)
            start = i + 1L
          case '\r' =>
            if (i + 1L < byteVector.size && byteVector.get(i + 1L) == '\n') {
              out = out :+ byteVector.slice(start, i)
              start = i + 2L
              i += 1L
            }
          case c =>
            ()
        }
        i += 1L
      }
      val carry = byteVector.slice(start, byteVector.size)
      (out, carry)
    }

    def extractLines(buffer: Vector[ByteVector],
                     chunk: Chunk[ByteVector],
                     pendingLineFeed: Boolean
                    ): (Chunk[ByteVector], Vector[ByteVector], Boolean) = {

      def loop(remainingInput: Vector[ByteVector],
               buffer: Vector[ByteVector],
               output: Vector[ByteVector],
               pendingLineFeed: Boolean
              ): (Chunk[ByteVector], Vector[ByteVector], Boolean) = {
        if (remainingInput.isEmpty) {
          (Chunk.indexedSeq(output), buffer, pendingLineFeed)
        } else {
          val next = remainingInput.head
          if (pendingLineFeed) {
            if (next(0L) == '\n') {
              val out : ByteVector = buffer.init.foldRight(buffer.last.init)(_ ++ _)
              loop(next.tail +: remainingInput.tail, Vector.empty, output :+ out, false)
            } else {
              loop(remainingInput, buffer, output, false)
            }
          } else {
            val (out, carry) = linesFromByteVector(next)
            val pendingLF = if (carry.nonEmpty) carry.last == '\r' else pendingLineFeed

            loop(remainingInput.tail,
              if (out.isEmpty) buffer :+ carry else Vector(carry),
              if (out.isEmpty) output else output ++ (buffer.foldRight(out.head)(_ ++ _) +: out.tail) , pendingLF
            )
          }
        }
      }

      loop(chunk.toVector, buffer, Vector.empty, pendingLineFeed)
    }

    def go(buffer: Vector[ByteVector], pendingLineFeed: Boolean): Handle[F, ByteVector] => Pull[F, ByteVector, Unit] = {
      _.receiveOption {
        case Some((chunk, h)) =>
          val (toOutput, newBuffer, newPendingLineFeed) = extractLines(buffer, chunk, pendingLineFeed)
          Pull.output(toOutput) >> go(newBuffer, newPendingLineFeed)(h)
        case None if buffer.nonEmpty => Pull.output1(buffer.foldLeft(ByteVector.empty)(_ ++ _))
        case None => Pull.done
      }
    }
    _.pull(go(Vector.empty, false))
  }

  private[multipart] def parseHeaders[F[_]]: Pipe[F, ByteVector, Either[Headers, ByteVector]] = _.map{ bv =>
    val headerM = for {
      line <- bv.decodeAscii.right.toOption
      idx <- Option(line indexOf ':')
      if idx >= 0
      if idx < line.length - 1
    } yield Header(line.substring(0, idx), line.substring(idx + 1).trim)
    Either.fromOption(headerM.map(Headers(_)), bv).swap
  }

  private[multipart] def addDelimiters[F[_]](delim: ByteVector):
    Pipe[F, Either[Headers, ByteVector], Either[Headers, ByteVector]] = {
    def go(optBV: Option[ByteVector]): Handle[F, Either[Headers, ByteVector]] => Pull[F, Either[Headers, ByteVector], Unit] = {
      _.receive1{
        case (r@Right(b1), h) =>
          h.receive1Option {
            case Some((Right(b2), h)) => optBV match {
              case Some(bv) =>
                Pull.output1(Either.right(bv ++ delim)) >> Pull.output1(Either.right(b1 ++ delim)) >> go(Option(b2))(h)
              case None =>
                Pull.output1(Either.right(b1 ++ delim)) >> go(Option(b2))(h)
            }
            case Some((l@Left(_), h)) => optBV match {
              case Some(bv) =>
                Pull.output1(Either.right(bv ++ delim)) >> Pull.output1(Either.right(b1)) >> Pull.output1(l) >> go(None)(h)
              case None =>
                Pull.output1(Either.right(b1)) >> Pull.output1(l) >> go(None)(h)
            }
            case None => optBV match {
              case Some(bv) => Pull.output1(Either.right(bv ++ delim)) >> Pull.output1(Either.right(b1))
              case None => Pull.output1(Either.right(b1))
            }
          }
        case (l@Left(_), h) => optBV match {
          case Some(bv) =>
            Pull.output1(Either.right(bv)) >> Pull.output1(l) >> go(None)(h)
          case None => Pull.output1(l) >> go(None)(h)
        }
      }
    }
    _.pull(go(None))
  }

  def addDelimiters(content: ByteVector, delim: ByteVector): ByteVector = content ++ delim
  def eitherAddDelimiters(content: Either[Headers, ByteVector], delim: ByteVector): Either[Headers, ByteVector] = {
    content.bimap(h => h, addDelimiters(_, delim))
  }


  def parse(boundary: Boundary): Pipe[Task, Byte, Either[Headers, Byte]] = { s =>
    val boundaryBytes = boundary.toByteVector
    val startLine = DASHDASH ++ boundaryBytes
    val endLine = startLine ++ DASHDASH

    // At various points, we'll read up until we find this, to loop back into beginPart.
    val expected = CRLF ++ startLine

    s.through(toByteVector)
      .through(lines)
      .filter(_ != startLine)
      .filter(_ != endLine)
      .through(parseHeaders)
      .map(eitherAddDelimiters(_, CRLF))
      .through(eitherByteVectorToBytes)
    
  }

}
