package org.http4s.multipart.rfc822


/**
  *
  * Every Header Should End By CRLF.
  *
  * If LWSP follows a CRLF rather than text then discard all LWSP and return only
  * the value.
  *
  * Generalized Header Field Definitions
  * field       =  field-name ":" [ field-body ] CRLF
  *
  * field-name  =  1*<any CHAR, excluding CTLs, SPACE, and ":">
  *
  * field-body  =  field-body-contents
  *                [CRLF LWSP-char field-body]
  *
  * field-body-contents =
  *               <the ASCII characters making up the field-body, as
  *                defined in the following sections, and consisting
  *                of combinations of atom, quoted-string, and
  *                specials tokens, or else consisting of texts>
  */
case class Header(name: String, value: String)


