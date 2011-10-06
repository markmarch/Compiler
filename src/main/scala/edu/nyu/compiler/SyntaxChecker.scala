package edu.nyu.compiler

import scala.io.Source
import java.io.FileNotFoundException

/**
 * Syntax checker that takes a file name as argument.
 * Will print "syntax error" if any syntax error is found,
 * otherwise no output.
 */
object SyntaxChecker extends TackParser {
  def main(args: Array[String]): Unit = {
    try {
      val s = Source.fromFile(args(0)).getLines.reduceLeft(_ + "\n" + _)
      val tokens = new PackratReader(new lexical.Scanner(s))
      phrase(program)(tokens) match{
        case Success(_,_) =>
        case e : NoSuccess => println("Syntax error : " + e.msg)
      }
    } catch {
      case e: FileNotFoundException => println(e.getMessage)
    }
  }
}