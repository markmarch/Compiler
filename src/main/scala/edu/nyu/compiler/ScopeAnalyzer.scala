package edu.nyu.compiler

import edu.nyu.compiler.ScopeAnalyzer.Result
import collection.mutable.ListBuffer

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

  def mkRecordType(list: List[Tuple2[String, Type]]) = {
    RecordType(list.map((r: Tuple2[String, Type]) => FieldType(FieldId(r._1), r._2)))
  }

  def int(id: String) = (id, PrimitiveType("int"))

  def string(id: String) = (id, PrimitiveType("string"))

  def bool(id: String) = (id, PrimitiveType("string"))

  val fieldPlaceHolder = List()
  val recordPlaceHolder = RecordType(fieldPlaceHolder)

  val intrinsicFunctionDes: List[Tuple3[String, Type, List[Tuple2[String, Type]]]] = List(
    ("append", "string", List(string("lhs"), string("rhs"))),
    ("bool2int", "int", List(bool("b"))),
    ("bool2string", "string", List(bool("b"))),
    ("int2bool", "bool", List(bool("b"))),
    ("int2string", "string", List(int("i"))),
    ("length", "int", List(string("s"))),
    ("newArray", ArrayType(recordPlaceHolder), List(int("eSize"), int("aSize"))),
    ("newRecord", recordPlaceHolder, List(int("rSize"))),
    ("print", "void", List(string("s"))),
    ("range", ArrayType("int"), List(int("start"), int("end"))),
    ("size", "int", List(("a", ArrayType(recordPlaceHolder)))),
    ("string2bool", "bool", List(string("s"))),
    ("string2int", "int", List(string("s"))),
    ("stringEqual", "bool", List(string("lhs"), string("rhs")))
  )

  val intrinsicFunctionList = intrinsicFunctionDes.map(
    (f: Tuple3[String, Type, List[Tuple2[String, Type]]]) => new TackSymbol(null, f._1, FunType(mkRecordType(f._3), f._2)))

  def analyzeScope(program: Program): Result = {
    val table = createSymbolTable(program)
    val result = new Result(table, new ListBuffer[String])
    program.funList.foreach(analyzeScope(_, result))
    result
  }

  def analyzeScope(node: AstNode, result: Result): Result = {
    var table = result.table
    node match {
      case fun: FunDef => {
        table.topLevel.define(new TackSymbol(fun, fun.id.name, fun.typ))
        table.push(new Scope(fun, table.currentScope))
        fun.typ.fromType.fieldTypeList.foreach(analyzeScope(_, result))
        (fun.typ.fromType :: fun.blockStmt.stmtList).foreach(analyzeScope(_, result))
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
        defineSymbol(result, new TackSymbol(varDef, varDef.varId.name, null))
        analyzeScope(varDef.expr, result)
      }
      case fieldType: FieldType => {
        defineSymbol(result, new TackSymbol(fieldType, fieldType.fieldId.name, fieldType.typ))
        analyzeScope(fieldType.typ, result)
      }
      case fieldLit: FieldLit => {
        defineSymbol(result, new TackSymbol(fieldLit, fieldLit.fieldId.name, null))
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