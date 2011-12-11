package edu.nyu.compiler

import io.Source
import java.io._

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/1/11
 * Time: 3:25 PM
 */

object Main extends TackParser with SemanticAnalyzer with IRGenerator{
  def main(args: Array[String]) {
    if (args.length == 0)
      printUsage()
    else if (args.length == 1)
      process(new File("test/resources/pr3-test/" + args(0)))
    else process(new File(args(0)), args(1).toInt)
  }

  def printUsage() {
    val usage = """Usage:
    | run <file> [milestone = 1, 2, 3, 4]
    | example:
    | run 012.tack   // print out generated IR code for 012.tack
    | run 012.tack 1 // check the syntax of 012.tack
    | run 012.tack 2 // print out the AST
    | run 012.tack 3 // check sematic of source 012.tack
    | run 012.tack 4 // print out generated IR code"""
    println(usage.stripMargin)
  }

  def writeToFile(lines: List[String], path: String) = {
    try {
      val file = new File(path)
      if (file.exists())
        file.delete()
      val writer = new BufferedWriter(new FileWriter(path))
      lines.foreach(line => writer.write(line + "\n"))
      writer.close()
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def process(file: File, mileStone: Int = 4) {
    try {
      val s = Source.fromFile(file).getLines().reduceLeft(_ + "\n" + _)
      val tokens = new PackratReader(new lexical.Scanner(s))
      val result = phrase(program)(tokens)
      mileStone match {
        case 1 => checkSyntax(result)
        case 2 => printAst(result)
        case 3 => semanticAnalyze(result)
        case 4 =>
          val code = generateIRCode(result)
          code.foreach(println)
          code match {
            case Nil =>
            case _ => writeToFile(code, file.getAbsolutePath.replace(".tack", ".ir"))
          }
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
        new SemanticAnalyzer {}.analyze(program)
      }
      case e: NoSuccess => println("Syntax error: " + e.msg)
    }
  }

  def generateIRCode(result: ParseResult[Program]): List[String] = {
    result match {
      case Success(program, _) => {
        generateIR(program)
      }
      case e: NoSuccess => println("Syntax error: " + e.msg); Nil
    }
  }
}
