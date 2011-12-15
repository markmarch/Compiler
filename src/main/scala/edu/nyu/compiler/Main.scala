package edu.nyu.compiler

import io.Source
import java.io._

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/1/11
 * Time: 3:25 PM
 */

object Main extends TackParser with TypeAnalyzer with IRGenerator with CodeGenerator {
  def main(args: Array[String]) {
    if (args.length == 0)
      printUsage()
    else if (args.length == 1)
      process(new File("test/resources/pr3-test/" + args(0)))
    else process(new File("test/resources/pr3-test/" + args(0)), args(1).toInt)
  }

  def printUsage() {
    val usage = """Usage:
    | run <file> [milestone = 1, 2, 3, 4]
    | example:
    | run 012.tack   // print out generated IR code for 012.tack
    | run 012.tack 1 // check the syntax of 012.tack
    | run 012.tack 2 // print out the AST
    | run 012.tack 3 // check sematic of source 012.tack
    | run 012.tack 4 // print out generated IR code
    | run 012.tack 5 // print out generated x64 assembly code"""
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

  def process(file: File, mileStone: Int = 5) {
    val s = Source.fromFile(file).getLines().reduceLeft(_ + "\n" + _)
    val tokens = new PackratReader(new lexical.Scanner(s))
    val result = phrase(program)(tokens)
    result match {
      case Success(program, _) => milestone(program, mileStone, file)
      case e: NoSuccess => println("Syntax error : " + e.msg)
    }
  }

  def milestone(p: Program, m: Int, file: File) {
    m match {
      case 1 =>
      case 2 => println(p.getStringRep(0))
      case 3 => analyzeType(p) match {
        case Right(_) => println(symbolTable.toString)
        case Left(e) => println(e)
      }
      case 4 => analyzeType(p) match {
        case Right(_) =>
          val code = genProgram(p).getStringRep
          code.foreach(println)
          writeToFile(code, file.getAbsolutePath.replace(".tack", ".ir"))
        case Left(e) => println(e)
      }
      case 5 => analyzeType(p) match {
        case Right(_) =>
          val irCode = genProgram(p)
          val code = generate(irCode)
          println(code);
          writeToFile(irCode.getStringRep, file.getAbsolutePath.replace(".tack", ".ir"))
          writeToFile(code.split("\n").toList, file.getAbsolutePath.replace(".tack", ".s"))
        case Left(e) => println(e)
      }
    }
  }
}
