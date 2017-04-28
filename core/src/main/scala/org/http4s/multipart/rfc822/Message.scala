package org.http4s.multipart.rfc822

import fs2._

class MessagePart {

  /**
    * Lexical Tokens
    *
    * The following rules are used to define an underlying lexical
    * analyzer,  which  feeds  tokens to higher level parsers.  See the
    * ANSI references, in the Bibliography.
    *
    * CHAR        =  <any ASCII character>        ; (  0-177,  0.-127.)
    * ALPHA       =  <any ASCII alphabetic character>
    *                                             ; (101-132, 65.- 90.)
    *                                             ; (141-172, 97.-122.)
    * DIGIT       =  <any ASCII decimal digit>    ; ( 60- 71, 48.- 57.)
    * CTL         =  <any ASCII control           ; (  0- 37,  0.- 31.)
    *                 character and DEL>          ; (    177,     127.)
    * CR          =  <ASCII CR, carriage return>  ; (     15,      13.)
    * LF          =  <ASCII LF, linefeed>         ; (     12,      10.)
    * SPACE       =  <ASCII SP, space>            ; (     40,      32.)
    * HTAB        =  <ASCII HT, horizontal-tab>   ; (     11,       9.)
    * <">         =  <ASCII quote mark>           ; (     42,      34.)
    * CRLF        =  CR LF
    *
    * LWSP-char   =  SPACE / HTAB                 ; semantics = SPACE
    *
    * linear-white-space =  1*([CRLF] LWSP-char)  ; semantics = SPACE
    *                                             ; CRLF => folding
    *
    * specials    =  "(" / ")" / "<" / ">" / "@"  ; Must be in quoted-
    *             /  "," / ";" / ":" / "\" / <">  ;  string, to use
    *             /  "." / "[" / "]"              ;  within a word.
    *
    * delimiters  =  specials / linear-white-space / comment
    *
    * text        =  <any CHAR, including bare    ; => atoms, specials,
    *                 CR & bare LF, but NOT       ;  comments and
    *                 including CRLF>             ;  quoted-strings are
    *                                             ;  NOT recognized.
    *
    * atom        =  1*<any CHAR except specials, SPACE and CTLs>
    *
    * quoted-string = <"> *(qtext/quoted-pair) <">; Regular qtext or
    *                                             ;   quoted chars.
    *
    * qtext       =  <any CHAR excepting <">,     ; => may be folded
    *                 "\" & CR, and including
    *                 linear-white-space>
    *
    * domain-literal =  "[" *(dtext / quoted-pair) "]"
    * dtext       =  <any CHAR excluding "[",     ; => may be folded
    *                 "]", "\" & CR, & including
    *                 linear-white-space>
    *
    * comment     =  "(" *(ctext / quoted-pair / comment) ")"
    *
    * ctext       =  <any CHAR excluding "(",     ; => may be folded
    *                 ")", "\" & CR, & including
    *                 linear-white-space>
    *
    * quoted-pair =  "\" CHAR                     ; may quote any char
    *
    * phrase      =  1*word                       ; Sequence of words
    *
    * word        =  atom / quoted-string
    */

  case class Body[F](bytes: Stream[F, Byte])

}
