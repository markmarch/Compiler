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
}