package edu.nyu.compiler

import edu.nyu.compiler.ScopeAnalyzer.Result
import collection.mutable.ListBuffer
import annotation.tailrec

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/1/11
 * Time: 2:52 PM
 */
object ScopeAnalyzer {

  class Result(val table: SymbolTable, val errors: ListBuffer[String])

}

trait ScopeAnalyzer {
  implicit def string2Type(str: String) = PrimitiveType(str)

  def mkRecordType(list: List[(String, Type)]) = {
    RecordType(list.map((r: (String, Type)) => FieldType(FieldId(r._1), r._2)))
  }

  def intType(id: String): (String, Type) = (id, PrimitiveType("int"))

  def stringType(id: String): (String, Type) = (id, PrimitiveType("string"))

  def boolType(id: String): (String, Type) = (id, PrimitiveType("string"))

  val fieldPlaceHolder = List()
  val recordPlaceHolder = RecordType(fieldPlaceHolder)

  val intrinsicFunctionDes: List[(String, Type, List[(String, Type)])] = List(
    ("append", int, List(stringType("lhs"), stringType("rhs"))),
    ("bool2int", int, List(boolType("b"))),
    ("bool2string", string, List(boolType("b"))),
    ("int2bool", bool, List(boolType("b"))),
    ("int2string", string, List(intType("i"))),
    ("length", int, List(stringType("s"))),
    ("newArray", ArrayType(recordPlaceHolder), List(intType("eSize"), intType("aSize"))),
    ("newRecord", recordPlaceHolder, List(intType("rSize"))),
    ("print", PrimitiveType("void"), List(stringType("s"))),
    ("range", ArrayType("int"), List(intType("start"), intType("end"))),
    ("size", int, List(("a", ArrayType(recordPlaceHolder)))),
    ("string2bool", bool, List(stringType("s"))),
    ("string2int", int, List(stringType("s"))),
    ("stringEqual", bool, List(stringType("lhs"), stringType("rhs")))
  )

  val intrinsicFunctionList = intrinsicFunctionDes.map(f => new TackSymbol(null, f._1, FunType(mkRecordType(f._3), f._2)))

  def analyzeScope(program: Program): Result = {
    val table = createSymbolTable(program)
    val result = new Result(table, new ListBuffer[String])
    program.funList.foreach(analyzeScope(_, result))
    result
  }

  def analyzeScope(node: AstNode, result: Result): Result = {
    val table = result.table
    node match {
      case fun: FunDef => {
        table.topLevel.define(new TackSymbol(fun, fun.id.name, fun.typ))
        table.push(new Scope(fun, table.currentScope))
        fun.typ.fromType.fieldTypeList.foreach(f => defineSymbol(result, new TackSymbol(f.fieldId, f.fieldId.name, f.typ)))
        // (fun.typ.fromType :: fun.blockStmt.stmtList).foreach(analyzeScope(_, result))
        fun.blockStmt.stmtList.foreach(analyzeScope(_, result))
        table.pop()
      }
      case record: RecordType => {
        table.push(new Scope(record, table.currentScope))
        record.fieldTypeList.foreach(analyzeScope(_, result))
        table.pop()
      }
      case recordLit: RecordLit => {
        table.push(new Scope(recordLit, table.currentScope))
        recordLit.fieldLitList.foreach(analyzeScope(_, result))
        table.pop()
      }
      case blockStmt: BlockStmt => {
        table.push(new Scope(blockStmt, table.currentScope))
        blockStmt.stmtList.foreach(analyzeScope(_, result))
        table.pop()
      }
      case forStmt: ForStmt => {
        table.push(new Scope(forStmt, table.currentScope))
        val varId = forStmt.varId
        defineSymbol(result, new TackSymbol(varId, varId.name, null))
        (forStmt.expr :: forStmt.blockStmt.stmtList).foreach(analyzeScope(_, result))
        table.pop()
      }
      case varDef: VarDef => {
        defineSymbol(result, new TackSymbol(varDef.varId, varDef.varId.name, null))
        analyzeScope(varDef.expr, result)
      }
      case fieldType: FieldType => {
        defineSymbol(result, new TackSymbol(fieldType.fieldId, fieldType.fieldId.name, fieldType.typ))
        analyzeScope(fieldType.typ, result)
      }
      case fieldLit: FieldLit => {
        defineSymbol(result, new TackSymbol(fieldLit.fieldId, fieldLit.fieldId.name, null))
        analyzeScope(fieldLit.expr, result)
      }
      case _ => {
        node.children.foreach(analyzeScope(_, result))
      }
    }
    result
  }

  def defineSymbol(r: Result, s: TackSymbol) {
    if (r.table.currentScope.contains(s.name)) {
      r.errors += "multiple definition of " + s.name
    } else {
      r.table.currentScope.define(s)
    }
  }

  def createSymbolTable(program: Program): SymbolTable = {
    val topLevelScope = new Scope(program, null);
    // add all intrinsic functions to top level
    intrinsicFunctionList.foreach(topLevelScope.define(_))
    new SymbolTable(topLevelScope)
  }
}