package edu.nyu.compiler

import scala.io.Source
import java.io.{FileFilter, File, FileNotFoundException}

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 10/17/11
 * Time: 6:10 PM
 */

object ASTPrinter extends TackParser {
  
  class TackFileFilter extends FileFilter {
    override def accept(f : File) : Boolean = {
      f.getName.endsWith(".tack")
    }
  }

  def main(args: Array[String]) = {
    print(getAst(new File(args(0))))
  }

  def getAst(file : File) : String = {
    try {
      val s = Source.fromFile(file).getLines().reduceLeft(_ + "\n" + _)
      val tokens = new PackratReader(new lexical.Scanner(s))
      phrase(program)(tokens) match {
        case Success(program, _) => program.getStringRep(0)
        case e: NoSuccess => "Syntax error : " + e.msg
      }
    } catch {
      case e: FileNotFoundException => e.getMessage
    }
  }
}