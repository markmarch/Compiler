package edu.nyu.compiler

import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/4/11
 * Time: 2:16 PM
 */

object SemanticAnalyzer extends ScopeAnalyzer{

  def analyze(program : Program) = {
    val result = analyzeScope(program)
    result.errors match {
      case list : ListBuffer[String] if list.isEmpty => println(result.table)
      case list => list.foreach(println)
    }
  }
}