package edu.nyu.compiler

import scala.io.Source
import java.io.FileNotFoundException

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 10/17/11
 * Time: 6:10 PM
 * To change this template use File | Settings | File Templates.
 */

object ASTPrinter extends TackParser {
  def main(args : Array[String]) = {
     try {
      val s = Source.fromFile(args(0)).getLines.reduceLeft(_ + "\n" + _)
      val tokens = new PackratReader(new lexical.Scanner(s))
      phrase(program)(tokens) match{
        case Success(program,_) => println(program.getStringRep(0))
        case e : NoSuccess => println("Syntax error : " + e.msg)
      }
    } catch {
      case e: FileNotFoundException => println(e.getMessage)
    }
  }
}