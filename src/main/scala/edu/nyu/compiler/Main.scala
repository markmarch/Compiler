package edu.nyu.compiler

import io.Source
import java.io.{FileNotFoundException, File}

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/1/11
 * Time: 3:25 PM
 */

object Main extends TackParser {

  def main(args: Array[String]) {
    val file = new File("test/resources/pr3-test/001.tack")
    try {
      val s = Source.fromFile(file).getLines().reduceLeft(_ + "\n" + _)
      val tokens = new PackratReader(new lexical.Scanner(s))
      val result = phrase(program)(tokens)
      val mileStone = 2
      mileStone match {
        case 1 => checkSyntax(result)
        case 2 => printAst(result)
        case 3 => semanticAnalyze(result)
      }
    } catch {
      case e: FileNotFoundException => println(e.getMessage)
    }
  }

  def checkSyntax(result: ParseResult[Program]) = result match {
    case Success(_, _) =>
    case e: NoSuccess => println("Syntax error : " + e.msg)
  }

  def printAst(result: ParseResult[Program]) = result match {
    case Success(program, _) => println(program.getStringRep(0))
    case e: NoSuccess => println("Syntax error: " + e.msg)
  }

  def semanticAnalyze(result: ParseResult[Program]) = result match {
    case Success(program, _) => println(program.getStringRep(0))
    case e: NoSuccess => println("Syntax error: " + e.msg)
  }
}