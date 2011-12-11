package edu.nyu.compiler

import collection.mutable.{Stack, HashMap, ListBuffer}
import annotation.tailrec


class TackSymbol(val definition: AstNode, val name: String, var typ: Type) {
  if (definition != null)
    definition.symbol = this
  var address : Address = _
  override def toString = name + ":" + typ.toString
}


class Scope(val owner: AstNode, val parent: Scope) {
  val indentation = "  ";
  val children = new ListBuffer[Scope]()
  val symbols = new HashMap[String, TackSymbol]()
  // insert the scope to the ast node
  owner.scope = this

  def define(symbol: TackSymbol) = {
    symbols += (symbol.name -> symbol)
  }
  def contains(symbolName: String) = symbols.keySet.contains(symbolName)

  def get(symbolName: String) = symbols.get(symbolName)

  def getStringRep(level: Int): String = {
    val symbolsDefined = symbols.keys.toList.sortWith(_ < _).mkString("{", ", ", "}")
    val scopeDefined = children.map(_.getStringRep(level + 1)).mkString("")
    val label = owner match {
      case forStmt: ForStmt => forStmt.label + " " + forStmt.varId.name
      case funDef: FunDef => funDef.label + " " + funDef.id.name
      case _ => owner.label
    }
    indentation * level + label + " " + symbolsDefined + "\n" + scopeDefined
  }
}

class SymbolTable(val topLevel: Scope) {
  var scopes = List(topLevel)
  var currentScope = topLevel
  var currnetFuction : FunDef = _
  def pop(): Scope = {
    val head = scopes.head
    scopes = scopes.tail
    currentScope = scopes.head
    head
  }

  def push(scope : Scope) {
    push(scope, true)
  }

  def push(scope: Scope, addToChildren : Boolean) {
    if (addToChildren)
      currentScope.children += scope
    currentScope = scope
    scopes = scope :: scopes
  }

  override def toString = topLevel.getStringRep(0)

  def lookup(name: String): Option[TackSymbol] = {
    @tailrec
    def lookupInScope(l: List[Scope]): Option[TackSymbol] = l match {
      case Nil => None
      case x :: xs if x.contains(name) => x.get(name)
      case x :: xs => lookupInScope(xs)
    }
    lookupInScope(scopes)
  }
  
  def lookupFunDef(scope : Scope) : Option[FunDef] = scope.owner match {
    case f : FunDef => Some(f)
    case p : Program => None
    case _ => lookupFunDef(scope.parent)
  }
}