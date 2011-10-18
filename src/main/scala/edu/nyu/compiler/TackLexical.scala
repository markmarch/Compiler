package edu.nyu.compiler

import util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

class TackLexical extends StdLexical{
  // added comments to whitespace to be ignored
  override def whitespace : Parser[Any] = rep (
      whitespaceChar
    | '#' ~ rep(chrExcept(EofCh, '\n'))
    | '#' ~ failure("unclosed comment"))

  // string literal parser
  def string : Parser[Token] = '\"' ~ rep('\\' ~ chrExcept('\n',EofCh) ^^ { case '\\' ~ ch => "\\" + ch }
    | chrExcept('\"','\n', EofCh)) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "")}

  // override the default token parser to accept string literal
  // and ignore single quotes string literals
  override def token : Parser[Token] =
    ( identChar ~ rep( identChar | digit )              ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | digit ~ rep( digit )                              ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }
    | string
    | EofCh                                             ^^^ EOF
    | '\"' ~> failure("unclosed string literal")
    | delim
    | failure("illegal character")
    )
}