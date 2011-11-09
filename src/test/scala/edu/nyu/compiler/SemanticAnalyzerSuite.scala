package edu.nyu.compiler

import org.scalatest.FunSuite
import java.io.{FileFilter, File}
import scala.io.Source
/**
* Created by IntelliJ IDEA.
* User: xiaomingjia
* Date: 11/9/11
* Time: 12:51 PM
*/

class SemanticAnalyzerSuite extends FunSuite {
  object Parser extends TackParser

  test("analyze all given sample test cases") {
    val fileList = new File("test/resources/pr3-test").listFiles(new FileFilter() {
      override def accept(file : File) = file.getName.endsWith(".tack")
    })

    def process(f : File)  {
      println(f.getName)
      import Parser._
      phrase(program)(new PackratReader(new lexical.Scanner(Source.fromFile(f).getLines.reduceLeft(_ + "\n" + _)))) match {
        case Success(program, _) => {
          val result =  new TypeAnalyzer().analyze(program)
          if (result.isRight) {
            val got = result.right.get.split("\n").toList
            val symsFile = new File(f.getAbsolutePath.replace("tack", "syms"))
            if (symsFile.exists) {
              val expected = Source.fromFile(symsFile).getLines.toList
              expected.zip(got).foreach(p => assert(p._1 === p._2))
            }
          }
        }
        case e : NoSuccess => println("Syntax error : " + e.msg)
      }
    }

    fileList.foreach(process)
  }
}