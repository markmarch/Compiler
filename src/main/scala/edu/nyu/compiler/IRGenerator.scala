package edu.nyu.compiler

import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/16/11
 * Time: 3:59 PM
 */

trait IRGenerator extends TypeAnalyzer{
  private var nextLabelIndex = 0
  private var nextTempIndex = 0
  private val relationOp = Set("==", "!=", ">=", ">", "<=", "<")

  def mkLabel() = {
    nextLabelIndex += 1
    "L" + nextLabelIndex
  }

  def mkTemp() = {
    nextTempIndex += 1
    "t" + nextTempIndex
  }

  def generateIR(p: Program): List[String] = {
    analyzeType(p) match {
      case Left(e) => e.foreach(println); Nil
      case Right(_) => gen(p)
    }
  }

  def gen(node: AstNode): List[String] = node match {
    case p: Program => p.funList.flatMap(gen)
    case f: FunDef =>
      symbolTable.push(f.scope, false)
      val code = genFunDef(f.id.name, f.typ, f.blockStmt.stmtList.flatMap(gen))
      symbolTable.pop()
      code
    case s: Stmt => genStmt(s)
  }

  def genFunDef(id: String, typ: FunType, stmt: List[String]) =
    id + " = fun" + typ.fromType.toString + " -> " + typ.returnType.toString :: stmt.map("  " + _)

  def genStmt(stmt: Stmt): List[String] = stmt match {
    case VarDef(VarId(id), expr) => genVarDef(id, expr)
    case AssignStmt(left, right) => genAssignment(left, right)
    case BlockStmt(stmtList) =>
      symbolTable.push(stmt.scope, false)
      val code = stmtList.flatMap(genStmt)
      symbolTable.pop()
      code
    case CallStmt(callExpr) => genCallExpr(callExpr.callee, callExpr.paramList)._2
    case ForStmt(varId, expr, bs) =>
      symbolTable.push(stmt.scope, false)
      val code = genForStmt(varId.name, expr, bs)
      symbolTable.pop()
      code
    case IfStmt(expr, bs, optBs) => genIfStmt(expr, bs, optBs)
    case ReturnStmt(optExpr) => genReturnStmt(optExpr)
  }

  def genIfStmt(expr: Expression, bs: BlockStmt, optBs: Option[BlockStmt]) = {
    val trueLabel = mkLabel()
    val (jumpingCode, c) = expr match {
      case InfixExpr(op, left, right) if relationOp.contains(op) =>
        val (a1, c1) = genExpr(left)
        val (a2, c2) = genExpr(right)
        ("if " + a1 + " " + op + " " + a2 + " goto " + trueLabel + ";", c1 ::: c2)
      case PrefixExpr("!", expr) =>
        val (a, c) = genExpr(expr)
        ("ifFalse " + a + " goto " + trueLabel + ";", c)
      case _ =>
        val (a, c) = genExpr(expr)
        ("if " + a + " goto " + trueLabel + ";", c)
    }
    val falseLabel = mkLabel()
    val code = new ListBuffer[String]
    code.appendAll(c)
    code.append(jumpingCode)
    code.append("goto " + falseLabel + ";")
    code.appendAll(trueLabel + ":" :: bs.stmtList.flatMap(genStmt))

    val optionCode = optBs match {
      case Some(s) => s.stmtList.flatMap(genStmt)
      case None => Nil
    }

    code.appendAll(falseLabel + ":" :: optionCode)
    code.toList
  }

  def genReturnStmt(optExpr: Option[Expression]): List[String] = optExpr match {
    case Some(e) =>
      val (a, c) = genExpr(e)
      c ::: List("return " + a + ";")
    case None => List("return" + ";")
  }

  def genWhileStmt(expr: Expression, bs: BlockStmt) = {
    val code = new ListBuffer[String]
    val begin = mkLabel()
    val after = mkLabel()
    val (a, c) = genExpr(expr)
    code.append(begin + ":")
    code.appendAll(c)
    code.append("ifFalse " + a + " goto " + after + ";")
    symbolTable.push(bs.scope)
    code.appendAll(bs.stmtList.flatMap(genStmt))
    symbolTable.pop()
    code.append("goto " + begin + ";")
    code.append(after + ":")
    code.toList
  }

  def genVarDef(id: String, expr: Expression) = {
    val symbol = symbolTable.lookup(id)
    val (address, code) = expr match {
      case RecordLit(fieldLitList) =>
        val typ = symbol.get.typ.asInstanceOf[RecordType]
        genRecordLit(typ, fieldLitList)
      case ArrayLit(exprList) =>
        val typ = symbol.get.typ.asInstanceOf[ArrayType]
        genArrayLit(typ, exprList)
      case _ => genExpr(expr)
    }
    code ::: List(id + " = " + address + ";")
  }


  def genAssignment(left: Expression, right: Expression): List[String] = {
    val (a1, c1) = genExpr(left)
    val (a2, c2) = genExpr(right)
    c1 ::: c2 ::: List(a1 + " = " + a2 + ";")
  }

  def genForStmt(id: String, expr: Expression, bs: BlockStmt) = {
    val code = new ListBuffer[String]
    val (a, c) = genExpr(expr)
    code.appendAll(c)
    val size = mkTemp()
    val index = mkTemp()
    code.append("param[0 : 1] = " + a + ";")
    code.append(size + " = call size : 1" + ";")
    code.append(index + " = 0;")
    val begin = mkLabel()
    val after = mkLabel()
    code.append(begin)
    code.append("if " + index + " >= " + size + " goto " + after + ";")
    code.append(id + " = " + a + "[" + index + "]" + ";")
    code.appendAll(bs.stmtList.flatMap(genStmt))
    code.append("goto " + begin + ";")
    code.append(after + ":")
    code.toList
  }

  def genExpr(expr: Expression): (String, List[String]) = expr match {
    case id: VarId => (id.name, Nil)
    case b : BoolLit => (b.value.toString, Nil)
    case s : StringLit => ("\"" + s.value + "\"", Nil)
    case i : IntLit => (i.value.toString, Nil)
    case n : NullLit => ("null", Nil)
    case c: CallExpr => genCallExpr(c.callee, c.paramList)
    case c: CastExpr => genCastExpr(c.expr, c.typ)
    case f: FieldExpr => genFieldExpr(f.expr, f.fieldId.name)
    case p: ParenExpr => genExpr(p.expr)
    case p: PrefixExpr => genPrefixExpr(p.op, p.expr)
    case i: InfixExpr => genInfixExpr(i.op, i.left,  i.right)
    case s: SubscriptExpr => genSubscriptExpr(s.left, s.right)
  }

  def genSubscriptExpr(left : Expression, right : Expression) : (String, List[String]) = {
    val (a1, c1) = genExpr(left)
    val (a2, c2) = genExpr(right)
    val temp = mkTemp()
    (temp, c1 ::: c2 ::: List(temp + " = " + a1 + "[" + a2 + "];"))
  }

  def genArrayLit(typ: ArrayType, exprList: List[Expression]): (String, List[String]) = {
    val code = new ListBuffer[String]

    val list = exprList.map(genExpr)
    code.appendAll(list.flatMap(_._2))

    val temp = mkTemp()
    code.append("param [0 : 2] = " + typ.typ.toString + ";")
    code.append("param [1 : 2] = " + exprList.size + ";")
    code.append(temp + " = call newArray : 2" + ";")

    val assignCode =
      for ((ele, i) <- list.map(_._1).zipWithIndex)
      yield temp + "[" + i + "] = " + ele + ";"
    code.appendAll(assignCode)
    (temp, code.toList)
  }

  def genRecordLit(typ: RecordType, fieldLitList : List[FieldLit]): (String, List[String]) = {
    val temp = mkTemp()
    val code = new ListBuffer[String]
    val list = fieldLitList.map(fieldLit => genExpr(fieldLit.expr))
    code.appendAll(list.flatMap(_._2))
    code.append("param[0 : 1] =" + typ.toString + ";")
    code.append(temp + " = call newRecord : 1;")
    val assignCode =
      for ((field, value) <- typ.fieldTypeList.map(_.fieldId.name).zip(list.map(_._1)))
      yield temp + "." + field + " = " + value + ";"
    code.appendAll(assignCode)
    (temp, code.toList)
  }

  def funCall(paramList: List[String]): List[String] = {
    val arity = paramList.size
    paramList.zipWithIndex.map(p => "param[" + p._2 + " : " + arity + "] = " + p._1 )
  }

  def genPrefixExpr(op: String, expr: Expression): (String, List[String]) = {
    val (a, c) = genExpr(expr)
    val temp = mkTemp()
    (temp, c ::: List(temp + " = " + op + " " + a))
  }

  def genInfixExpr(op: String, left: Expression, right: Expression) = {
    val (a1, c1) = genExpr(left)
    val (a2, c2) = genExpr(right)
    val temp = mkTemp()
    (temp, c1 ::: c2 ::: List(temp + " = " + a1 + " " + op + " " + a2 ))
  }


  def genCallExpr(callee: FunId, params: List[Expression]): (String, List[String]) = {
    val arity = params.size
    val list = params.map(genExpr)
    val prepareParamCode =
      for ((a, i) <- list.map(_._1).zipWithIndex)
        yield "param[" + i + " : " + arity + "] = " + a + ";"
    val callCode = "call " + callee.name + " : " + arity + ";"
    ("", list.flatMap(_._2) ::: prepareParamCode ::: List(callCode))
  }

  def genCastExpr(expr: Expression, typ: Type) = {
    val (a, code) = genExpr(expr)
    (a, code ::: List(a + ":" + typ.toString))
  }

  def genFieldExpr(expr: Expression, field: String) = {
    val (a, code) = genExpr(expr)
    (a + "." + field, code)
  }
}