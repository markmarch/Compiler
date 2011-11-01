package edu.nyu.compiler

import collection.mutable.{Stack, HashMap, ListBuffer}


class Symbol(val definition : AstNode, val name : String, val typ : Type)

class Scope(val owner : AstNode, val parent : Scope) {
  val children = new ListBuffer[Scope]()
  val symbols = new HashMap[String, Symbol]()
  
  def define(symbol : Symbol) = symbols += (symbol.name -> symbol)

  def contains(symbolName : String) = symbols.keySet.contains(symbolName)

  def get(symbolName : String) = symbols.get(symbolName)

  def print(indent : Int) = {

  }
}

class SymbolTable(val topLevel : Scope){
  val scopes = new Stack[Scope]()
  val currentScope = topLevel
  
  scopes.push(topLevel)
  
  def pop() : Scope = scopes.pop()
  
  def push(scope : Scope) = scopes.push(scope)

  def print() = {}

}