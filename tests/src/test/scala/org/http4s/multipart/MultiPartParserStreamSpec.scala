package org.http4s.multipart

import fs2.Stream._
import fs2.{Stream, Task}
import org.http4s.Http4sSpec
import org.http4s.multipart.MultipartParser._
import scodec.bits.ByteVector

/**
  * Created by davenpcm on 2/18/17.
  */
class MultiPartParserStreamSpec extends Http4sSpec  {
  def ruinDelims(str: String) = augmentString(str) flatMap {
    case '\n' => "\r\n"
    case c => c.toString
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

      def unspool(str: String): Stream[Task, Byte] = {
        if (str.length <= Limit) {
          emits(str getBytes "ASCII")
        } else {
          val front = str.substring(0, Limit)
          val back = str.substring(Limit)

          emits(front getBytes "ASCII") ++ unspool(back)
        }
      }

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

      def bvToString(byteVector: ByteVector): String = byteVector.foldLeft("")(_ + _.toChar)

      val byteVectorStream = unspool(input).through(toByteVector)

      val lineStream = byteVectorStream.through(extractlines).runLog.map(_.map(_.decodeAscii)).unsafeRun()

      lineStream.foreach(println)


      lineStream must_== expected
    }
  }

}
