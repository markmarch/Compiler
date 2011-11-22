package edu.nyu.compiler

import collection.mutable.ListBuffer
import java.lang.Object

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/4/11
 * Time: 2:16 PM
 */

trait SemanticAnalyzer extends TypeAnalyzer {
  def analyze(program : Program) = {
    analyzeType(program) match {
      case Right(s) => println(s)
      case Left(e) => println(e); System.exit(1)
    }
  }
}