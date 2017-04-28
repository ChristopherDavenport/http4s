package org.http4s.multipart.rfc1341

import java.nio.charset.StandardCharsets

import fs2._
import org.http4s.headers.`Content-Disposition`
import org.http4s.util.CaseInsensitiveString
import org.http4s.{EntityBody, Headers}

sealed trait MultipartComponents



/**
  * The only mandatory parameter for the multipart  Content-Type
  * is  the  boundary  parameter,  which  consists  of  1  to 70
  * characters from a set of characters known to be very  robust
  * through  email  gateways,  and  NOT ending with white space.
  * (If a boundary appears to end with white  space,  the  white
  * space  must be presumed to have been added by a gateway, and
  * should  be  deleted.)   It  is  formally  specified  by  the
  * following BNF:
  *
  * boundary := 0-69<bchars> bcharsnospace
  *
  * bchars := bcharsnospace / " "
  *
  * bcharsnospace :=    DIGIT / ALPHA / "'" / "(" / ")" / "+"  /
  *                     "_" / "," / "-" / "." / "/" / ":" / "=" / "?"
  *
  * @param value Boundary String Meeting The Above Criteria
  */
final case class Boundary(value: String) extends MultipartComponents

/**
  * A fully parsed Multipart body.
  *
  * multipart-body := preamble 1*encapsulation close-delimiter epilogue
  */
final case class MultipartBody(preamble: Preamble, parts: List[Encapsulation], closeDelimiter: CloseDelimiter, epilogue: Epilogue)

/**
  * An Encapsulation Is the Opening Delimiter Followed By a CRLF and then a Body Part
  *
  *  encapsulation := delimiter CRLF body-part
  */
final case class Encapsulation(delimiter: Delimiter, myPart: Part) extends MultipartComponents

/**
  * The Delimiter to Separate Parts
  * delimiter := CRLF "--" boundary   ; taken from  Content-Type field.
  *                                   ; when content-type is multipart
  *                                   ; There must be no space
  *                                   ; between "--" and boundary.
  * @param boundary The Boundary from Content-Type Header
  */
final case class Delimiter(boundary: Boundary) extends MultipartComponents

/**
  * close-delimiter := delimiter "--" ; Again, no  space  before "--"
  * @param boundary The Boundary from Content-Type Header
  */
final case class CloseDelimiter(boundary: Boundary) extends MultipartComponents

/**
  * The preamble text, to be completely ignored
  * preamble :=  *text                ;  to be ignored upon receipt.
  * @param text The text to be ignored.
  */
final case class Preamble(text: String) extends MultipartComponents

/**
  * The epilogue text, to be completely ignored
  * epilogue :=  *text                  ;  to be ignored upon receipt.
  * @param text The text to be ignored.
  */
final case class Epilogue(text: String) extends MultipartComponents

/**
  *
  * body-part = <"message" as defined in RFC 822,
  * with all header fields optional, and with the
  * specified delimiter not occurring anywhere in
  * the message body, either on a line by itself
  * or as a substring anywhere.  Note that the
  * semantics of a part differ from the semantics
  * of a message, as described in the text.>
  * RFC 822 - https://tools.ietf.org/html/rfc822
  *
  * @param headers
  * @param body The Body of the Message. EntityBody is type Stream[Task, Byte]
  */
final case class Part(headers: Headers, body: EntityBody) extends MultipartComponents
//{
//  def name: Option[CaseInsensitiveString] = headers.get(`Content-Disposition`).map(_.name)
//}

/**
  * The characters "--"
  */
case object DashDash extends MultipartComponents

/**
  * The CRLF is a linebreak of the form "\r\n"
  */
case object CRLF extends MultipartComponents


