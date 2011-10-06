package edu.nyu.compiler

import scala.io.Source
import java.io.FileNotFoundException
import scala.util.parsing.combinator.PackratParsers

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 10/5/11
 * Time: 8:54 PM
 * To change this template use File | Settings | File Templates.
 */

object SyntaxChecker extends TackParser{
  def main(args: Array[String]): Unit = {


    def addZero(n: Int) = "0" * (3 - n.toString.length) + n.toString

    val files = ((1 to 20).map(addZero(_))).toList ::: List("022","023","024")
    for (file <- files) {
      try {
        println(file)
        val s = Source.fromFile("test/" + file + ".tack").getLines.reduceLeft(_ + "\n" + _)
        val tokens = new PackratReader(new lexical.Scanner(s))
        println(phrase(program)(tokens))
      } catch {
        case e: FileNotFoundException =>
      }
    }
  }
}