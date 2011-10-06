package edu.nyu.compiler

import util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 10/5/11
 * Time: 10:37 AM
 * To change this template use File | Settings | File Templates.
 */

class TackLexical extends StdLexical{
  override def whitespace : Parser[Any] = rep (
      whitespaceChar
    | '#' ~ rep(chrExcept(EofCh, '\n'))
    | '#' ~ failure("unclosed comment"))

  def string : Parser[Token] = '\"' ~ rep(('\\' ~ chrExcept('\n',EofCh))
    | chrExcept('\"','\n', EofCh)) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "")}

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