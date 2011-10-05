package edu.nyu.compiler

import util.parsing.combinator.syntactical.StandardTokenParsers

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 10/5/11
 * Time: 10:14 AM
 */
object TackParser extends StandardTokenParsers {
  override val lexical = new TackLexical
  def main(args : Array[String]) : Unit = {
    def printTokens(s : lexical.Scanner) : Unit= {
      s match {
        case _ if s.atEnd =>
        case _ => println(s.first); printTokens(s.rest)
      }
    }
    val tokens = new lexical.Scanner("#This is a comment\n abc")
    printTokens(tokens)
  }
}