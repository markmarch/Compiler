package edu.nyu.compiler

import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/4/11
 * Time: 2:16 PM
 */

object SemanticAnalyzer{

  def analyze(program : Program) {
    new TypeAnalyzer().analyze(program)
    println("---------------------------")
  }
}