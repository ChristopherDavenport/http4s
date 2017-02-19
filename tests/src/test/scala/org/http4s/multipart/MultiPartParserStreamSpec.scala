package org.http4s
package multipart

import fs2.Stream._
import fs2.{Stream, Task}
import org.http4s.Http4sSpec
import org.http4s.multipart.MultipartParser._
import scodec.bits.ByteVector
import org.http4s.headers._

/**
  * Created by davenpcm on 2/18/17.
  */
class MultiPartParserStreamSpec extends Http4sSpec  {

  def ruinDelims(str: String): String = augmentString(str) flatMap {
    case '\n' => "\r\n"
    case c => c.toString
  }

  def unspool(str: String, limit: Int): Stream[Task, Byte] = {
    if (str.length <= limit) {
      emits(str getBytes "ASCII")
    } else {
      val front = str.substring(0, limit)
      val back = str.substring(limit)

      emits(front getBytes "ASCII") ++ unspool(back, limit)
    }
  }

  "receiveLine" should {
    "produce the lines of content" in {

      val Limit = 15
      val unprocessedInput = """--_5PHqf8_Pl1FCzBuT5o_mVZg36k67UYI
                               |Content-Disposition: form-data; name="upload"; filename="integration.txt"
                               |Content-Type: application/octet-stream
                               |Content-Transfer-Encoding: binary
                               |
                               |this is a test
                               |here's another test
                               |catch me if you can!
                               |
                               |--_5PHqf8_Pl1FCzBuT5o_mVZg36k67UYI--""".stripMargin

      val input = ruinDelims(unprocessedInput)


      val expected = Vector(
        "--_5PHqf8_Pl1FCzBuT5o_mVZg36k67UYI",
        """Content-Disposition: form-data; name="upload"; filename="integration.txt"""",
        "Content-Type: application/octet-stream",
        "Content-Transfer-Encoding: binary",
        "",
        "this is a test",
        "here's another test",
        "catch me if you can!",
        "",
        "--_5PHqf8_Pl1FCzBuT5o_mVZg36k67UYI--"
      ).map(Either.right)

      val byteVectorStream = unspool(input, Limit).through(toByteVector)

      val lineStream = byteVectorStream.through(lines).runLog.map(_.map(_.decodeAscii)).unsafeRun()

      lineStream must_== expected
    }
  }

  "headers" should {
    "correctly parse bare headers" in {
      val contentLength = 50L
      val unprocessedInput =
        s"""Content-Type: application/octet-stream
          |Content-Length: ${contentLength.toString}""".stripMargin

      val input = ruinDelims(unprocessedInput)
      val byteVectorStream = unspool(input, 15).through(toByteVector)
      val headerStream = byteVectorStream.through(lines).through(parseHeaders)

      val expected : Vector[Either[Headers, ByteVector]] = Vector(
        Headers(`Content-Type`(MediaType.`application/octet-stream`)),
        Headers(`Content-Length`(contentLength))
      ).map(Either.left)

      headerStream.runLog.unsafeRun() must_== expected
    }

  }
}
