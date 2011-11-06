package edu.nyu.compiler

sealed abstract class AstNode {
  var scope: Scope = _
  val indent = "  " // two space indentation
  def label : String = this.getClass.getSimpleName
  def children : List[AstNode] = Nil
  def getStringRep(level : Int) : String= {
    val childrenString = children.map{_.getStringRep(level + 1)}.mkString("")
    indent * level + label + "\n" + childrenString
  }
}

sealed abstract class Expression extends AstNode

sealed abstract class Type extends AstNode

sealed abstract class Stmt extends AstNode

trait HasValue[T] {
  def value : T
}

trait HasName {
  def name : String
}

case class BoolLit (value : Boolean) extends Expression with HasValue[Boolean]{
  override def label = "BoolLit " + value
}

case class StringLit (value : String) extends Expression with HasValue[String] {
  override def label = "StringLit \"" + value + "\""
}

case class IntLit(value : Int) extends Expression with HasValue[Int]{
  override def label = "IntLit " + value
}

case class NullLit() extends Expression

case class ArrayLit(exprList : List[Expression]) extends Expression {
  override def children = exprList
}

case class RecordLit(fieldLitList : List[FieldLit]) extends Expression {
  override def children = fieldLitList
}

case class FieldLit(fieldId : FieldId, expr : Expression) extends Expression {
  override def children = List(fieldId, expr)
}

case class FunId(name : String) extends Expression with HasName {
  override def label = "FunId " + name
}

case class VarId(name : String) extends Expression with HasName {
  override def label = "VarId " + name
}

case class FieldId(name : String) extends AstNode with HasName {
  override def label = "FieldId " + name
}

case class InfixExpr(op : String, left : Expression, right : Expression) extends Expression {
  override def label = "InfixExpr " + op
  override def children = List(left, right)
}

case class PrefixExpr(op : String,  expr : Expression) extends Expression {
  override def label = "PrefixExpr " + op
  override def children = List(expr)
}

case class CallExpr(callee : FunId, paramList : List[Expression]) extends Expression {
  override def children = callee :: paramList
}

case class CastExpr(expr : Expression, typ : Type) extends Expression  {
  override def children = List(expr, typ)
}

case class FieldExpr(expr : Expression, fieldId : FieldId) extends Expression {
  override def children = List(expr, fieldId)
}

case class SubscriptExpr(left : Expression, right : Expression) extends Expression {
  override def children = List(left, right)
}

case class ParenExpr(expr : Expression) extends Expression {
  override def children = List(expr)
}

case class Program(funList : List[FunDef]) extends AstNode {
  override def children = funList
}

case class FunDef(id : FunId, typ : FunType, blockStmt : BlockStmt) extends AstNode {
  override def children = List(id, typ, blockStmt)
}

case class RecordType(fieldTypeList : List[FieldType]) extends Type {
  override def children = fieldTypeList
  
  override def toString = fieldTypeList.map(f => f.toString).mkString("(", ",", ")")
}

case class FunType(fromType : RecordType, returnType : Type) extends Type  {
  override def children = List(fromType, returnType)
}

case class ArrayType(typ : Type) extends Type {
  override def children = List(typ)
  
  override def toString = "[" + typ.toString + "]"
}

case class FieldType(fieldId : FieldId, typ : Type)  extends Type {
  override def children = List(fieldId, typ)

  override def toString = fieldId.name + " : " + typ.toString
}

case class NullType() extends Type {
  override def toString = "null"
}

case class PrimitiveType (name : String) extends Type with HasName {
  override def label = "PrimitiveType " + name

  override def toString = name
}

case class VarDef(varId : VarId, expr : Expression) extends Stmt {
  override def children = List(varId, expr)
}

case class AssignStmt(left : Expression, right : Expression) extends Stmt {
  override def children = List(left, right)
}

case class BlockStmt(stmtList : List[Stmt]) extends Stmt {
  override def children = stmtList
}

case class CallStmt(expr : Expression) extends Stmt  {
  override def children = List(expr)
}

case class ForStmt(varId : VarId,  expr : Expression, blockStmt : BlockStmt) extends Stmt {
  override def children = List(varId, expr, blockStmt)
}

case class IfStmt(expr : Expression, blockStmt : BlockStmt, optBlockStmt : Option[BlockStmt]) extends Stmt {
  override def children = List(expr, blockStmt) ::: optBlockStmt.toList
}

case class ReturnStmt(optExpr : Option[Expression]) extends Stmt {
  override def children = optExpr.toList
}

case class WhileStmt(expr : Expression, blockStmt : BlockStmt) extends Stmt {
  override def children = List(expr, blockStmt)
}