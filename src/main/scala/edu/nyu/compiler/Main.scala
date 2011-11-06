package edu.nyu.compiler

import io.Source
import java.io.{FileFilter, FileNotFoundException, File}

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/1/11
 * Time: 3:25 PM
 */

object Main extends TackParser {

  def main(args: Array[String]) {
    val dir = "test/resources/pr3-test/"
    // val fileList = List("012", "013", "014", "015", "016").map(name => new File(dir + name + ".tack"))
    val fileList = new File("test/resources/pr3-test").listFiles(new FileFilter(){
      override def accept(f : File)  = {
        f.getName.endsWith(".tack")
      }
    })
    // val fileList = List(new File("test/sample.tack"))
    if (args == null || args.length == 0)
      fileList.foreach(process)
    else 
      process(new File(dir + args(0)))
  }

  def process(file: File) {
    try {
      println(file.getName)
      val s = Source.fromFile(file).getLines().reduceLeft(_ + "\n" + _)
      val tokens = new PackratReader(new lexical.Scanner(s))
      val result = phrase(program)(tokens)
      val mileStone = 3
      mileStone match {
        case 1 => checkSyntax(result)
        case 2 => printAst(result)
        case 3 => semanticAnalyze(result)
      }
    } catch {
      case e: FileNotFoundException => println(e.getMessage)
    }
  }

  def checkSyntax(result: ParseResult[Program]) {
    result match {
      case Success(_, _) =>
      case e: NoSuccess => println("Syntax error : " + e.msg)
    }
  }

  def printAst(result: ParseResult[Program]) {
    result match {
      case Success(program, _) => println(program.getStringRep(0))
      case e: NoSuccess => println("Syntax error: " + e.msg)
    }
  }

  def semanticAnalyze(result: ParseResult[Program]) {
    result match {
      case Success(program, _) => {
        SemanticAnalyzer.analyze(program)
      }
      case e: NoSuccess => println("Syntax error: " + e.msg)
    }
  }
}