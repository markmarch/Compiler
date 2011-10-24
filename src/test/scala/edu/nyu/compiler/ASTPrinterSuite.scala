package edu.nyu.compiler

import org.scalatest.FunSuite
import java.io.File
import io.Source
import edu.nyu.compiler.ASTPrinter.TackFileFilter

class ASTPrinterSuite extends FunSuite{
  test("printAst") {
    val path = "test/resources/pr2-test"
    val fileNames = (1 to 20).map((number) => "0" * (3 - number.toString.length) + number)
    for{
      name <- fileNames ; 
      val srcFile = new File(path + "/" + name + ".tack")
      if srcFile.exists()
    } {
      check(srcFile, new File(path + "/" + name + ".ast"))
    }
  }


  def check(srcFile : File, correctAstFile : File) = {
    println("checking " + srcFile.getName)
    val result = ASTPrinter.getAst(srcFile).split("\n").toList
    val expectedLines = Source.fromFile(correctAstFile).getLines().filter(_.length > 0).toList
    val comparsion = expectedLines.zip(result)
    comparsion.foreach((pair) => assert(pair._1 === pair._2))
  }
}