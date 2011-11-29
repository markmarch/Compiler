package edu.nyu.compiler

import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/16/11
 * Time: 3:59 PM
 */

trait IRGenerator extends TypeAnalyzer {
  private var nextLabelIndex = 0
  private var nextTempIndex = 0
  private val relationOp = Set("==", "!=", ">=", ">", "<=", "<")
  private val arithmeticOp = Set("+", "-", "*", "/", "%")

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
      case Left(e) => println(e); Nil
      case Right(_) => gen(p)
    }
  }

  def gen(p: Program): List[String] = p.funList.flatMap(genFunDef)

  def genFunDef(f: FunDef) = {
    genStmt(f.blockStmt)
    f.id.name + " = fun" + f.typ.fromType.toString + " -> " + f.typ.returnType.toString :: f.blockStmt.code.map("  " + _)
  }

  def genStmt(stmt: Stmt): Unit = stmt match {
    case v: VarDef => genVarDef(v)
    case a: AssignStmt => genAssignStmt(a)
    case b: BlockStmt => genBlockStmt(b)
    case c: CallStmt => genCallStmt(c)
    case f: ForStmt => genForStmt(f)
    case i: IfStmt => genIfStmt(i)
    case r: ReturnStmt => genReturnStmt(r)
    case w: WhileStmt => genWhileStmt(w)
  }

  def genCallStmt(c: CallStmt) {
    genCallExpr(c.expr)
    c.code = c.expr.code ::: List(c.expr.address)
  }

  def genBlockStmt(b: BlockStmt) {
    b.stmtList.foreach(genStmt)
    b.code = b.stmtList.flatMap(_.code)
  }

  def genIfStmt(i: IfStmt) {
    i.expr.t = mkLabel()
    i.expr.f = mkLabel()
    genJumpingCode(i.expr)
    genStmt(i.blockStmt)

    val code = new ListBuffer[String]
    code.appendAll(i.expr.code)
    code.append(i.expr.t + ":")
    code.appendAll(i.blockStmt.code)
    code.append(i.expr.f + ":");
    i.optBlockStmt match {
      case Some(b) => genStmt(b); code.appendAll(b.code)
      case None =>
    }
    i.code = code.toList
  }

  def genReturnStmt(r: ReturnStmt) {
    r.optExpr match {
      case Some(e) =>
        genValueCode(e)
        r.code = e.code ::: List("return " + e.address + ";")
      case None => r.code = List("return" + ";")
    }
  }

  def genWhileStmt(w: WhileStmt) {
    w.before = mkLabel()
    w.after = mkLabel()
    w.expr.t = mkLabel()
    w.expr.f = w.after
    genJumpingCode(w.expr)
    genStmt(w.blockStmt)
    val code = new ListBuffer[String]
    code.append(w.before + ":")
    code.appendAll(w.expr.code)
    code.append(w.expr.t)
    code.appendAll(w.blockStmt.code)
    code.append("goto " + w.before)
    code.append(w.after + ":")
  }

  def genVarDef(v: VarDef) {
    genAssignment(v, v.varId, v.expr)
  }

  def genAssignment(s: Stmt, from: Expression, to: Expression) {
    genValueCode(from)

    to match {
      case InfixExpr(op, left, right) if arithmeticOp.contains(op) =>
        genValueCode(left)
        genValueCode(right)
        s.code = from.code ::: left.code :::
          right.code ::: List(from.address + " = " + left.address + " " + op + " " + right.address + ";")
      case PrefixExpr(op, e) =>
        genValueCode(e)
        s.code = from.code ::: e.code :::
          List(from.address + " = " + op + e.address)
      case _ =>
        genValueCode(to)
        s.code = from.code ::: to.code ::: List(from.address + " = " + to.address)
    }
  }

  def genAssignStmt(a: AssignStmt) {
    genAssignment(a, a.left, a.right)
  }

  def genForStmt(f: ForStmt) = {
    genValueCode(f.expr)
    val code = new ListBuffer[String]
    code.appendAll(f.expr.code)
    val size = mkTemp()
    val index = mkTemp()
    code.append("param[0 : 1] = " + f.expr.address + ";")
    code.append(size + " = call size : 1" + ";")
    code.append(index + " = 0;")
    val begin = mkLabel()
    val after = mkLabel()
    code.append(begin)
    code.append("if " + index + " >= " + size + " goto " + after + ";")
    code.append(f.varId.name + " = " + f.expr.address + "[" + index + "]" + ";")
    f.blockStmt.stmtList.foreach(genStmt)
    code.appendAll(f.blockStmt.stmtList.flatMap(_.code))
    code.append("goto " + begin + ";")
    code.append(after + ":")
    f.code = code.toList
  }

  def genJumpingCode(expr: Expression): Unit = {
    expr match {
      case InfixExpr("||", left, right) =>
        left.t = expr.t
        left.f = mkLabel()
        right.t = expr.t
        right.f = expr.f
        genJumpingCode(left)
        genJumpingCode(right)
        expr.code = left.code ::: List(left.f + ":") ::: right.code
      case InfixExpr("&&", left, right) =>
        left.t = mkLabel()
        left.f = expr.f
        right.t = expr.t
        right.f = expr.f
        genJumpingCode(left)
        genJumpingCode(right)
        expr.code = left.code ::: List(left.t + ":") ::: right.code
      case InfixExpr(op, left, right) if relationOp.contains(op) =>
        genValueCode(left)
        genValueCode(right)
        expr.code = left.code ::: right.code :::
          List("if " + left.address + " " + op + " " + right.address + " goto " + expr.t + ";",
            "goto " + expr.f + ";")
      case PrefixExpr("!", e) =>
        e.t = expr.f
        e.f = expr.t
        genJumpingCode(e)
        expr.code = e.code
      case BoolLit(true) =>
        expr.code = List("goto " + expr.t + ";")
      case BoolLit(false) =>
        expr.code = List("goto " + expr.f + ";")
    }
  }

  def genValueCode(expr: Expression): Unit = expr match {
    case i: VarId => i.address = i.name
    case b: BoolLit => b.address = b.value.toString
    case s: StringLit => s.address = "\"" + s.value + "\""
    case i: IntLit => i.address = i.value.toString
    case n: NullLit => n.address = "null"
    case a: ArrayLit => genArrayLit(a)
    case r: RecordLit => genRecordLit(r)
    case f: FieldLit => genFieldLit(f)
    case c: CallExpr => genCallExpr(c)
    case c: CastExpr => genCastExpr(c)
    case f: FieldExpr => genFieldExpr(f)
    case p: ParenExpr => genParenExpr(p)
    case p: PrefixExpr => genPrefixExpr(p)
    case i: InfixExpr => genInfixExpr(i)
    case s: SubscriptExpr => genSubscriptExpr(s)
  }

  def genParenExpr(p: ParenExpr) {
    genValueCode(p.expr)
    p.address = p.expr.address
    p.code = p.expr.code
  }

  def genSubscriptExpr(subExpr: SubscriptExpr) {
    genValueCode(subExpr.left)
    genValueCode(subExpr.right)
    subExpr.address = subExpr.left.address + "[" + subExpr.right.address + "]"
    subExpr.code = subExpr.left.code ::: subExpr.right.code
  }

  def genArrayLit(a: ArrayLit) = {
    val typ = a.typ
    val code = new ListBuffer[String]
    a.exprList.foreach(genValueCode)

    code.appendAll(a.exprList.flatMap(_.code))

    val temp = mkTemp()
    val eleSize = mkTemp()
    code.append(eleSize + " = sizeof(" + typ.elementType.toString + ");")
    code.append("param [0 : 2] = " + eleSize + ";")
    code.append("param [1 : 2] = " + a.exprList.size + ";")
    code.append(temp + " = call newArray : 2" + ";")

    val assignCode =
      for ((ele, i) <- a.exprList.map(_.address).zipWithIndex)
      yield temp + "[" + i + "] = " + ele + ";"
    code.appendAll(assignCode)

    a.address = temp
    a.code = code.toList
  }

  def genRecordLit(r: RecordLit) {
    val typ = r.typ
    val fieldLitList = r.fieldLitList
    val temp = mkTemp()
    val code = new ListBuffer[String]
    fieldLitList.foreach(genValueCode)
    code.appendAll(fieldLitList.flatMap(_.code))
    code.append("param[0 : 1] =" + typ.toString + ";")
    code.append(temp + " = call newRecord : 1;")
    val assignCode =
      for ((field, value) <- typ.fieldTypeList.map(_.fieldId.name).zip(fieldLitList.map(_.address)))
      yield temp + "." + field + " = " + value + ";"
    code.appendAll(assignCode)
    r.address = temp
    r.code = code.toList
  }

  def genFieldLit(f: FieldLit) {
    genValueCode(f.expr)
    f.address = f.expr.address
    f.code = f.expr.code
  }

  def genPrefixExpr(prefixExpr: PrefixExpr) {
    genValueCode(prefixExpr.expr)
    prefixExpr.address = prefixExpr.op + prefixExpr.expr.address
    prefixExpr.code = prefixExpr.expr.code
  }

  def genInfixExpr(i: InfixExpr) {
    i.op match {
      case op if arithmeticOp.contains(op) =>
        genValueCode(i.left)
        genValueCode(i.right)
        i.address = mkTemp()
        i.code = i.left.code ::: i.right.code :::
          List(i.address + " = " + i.left.address + " " + i.op + " " + i.right.address + ";")
      case _ =>
        i.t = mkLabel()
        i.f = mkLabel()
        genJumpingCode(i)
        val jumpingCode = i.code
        i.address = mkTemp()
        val code = new ListBuffer[String]
        code.append(i.address + " = true;")
        code.appendAll(jumpingCode)
        code.append(i.f + ":")
        code.append(i.address + " = false;")
        code.append(i.t + ":")
        i.code = code.toList
    }
  }

  def genCallExpr(expr: CallExpr) {
    val callee = expr.callee
    val params = expr.paramList
    val arity = params.size

    params.foreach(genValueCode)
    val prepareParamCode =
      for ((a, i) <- params.map(_.address).zipWithIndex)
      yield "param[" + i + " : " + arity + "] = " + a + ";"
    expr.address = "call " + callee.name + " : " + arity + ";"
    expr.code = params.flatMap(_.code) ::: prepareParamCode;
  }

  def genCastExpr(castExpr: CastExpr) {
    genValueCode(castExpr.expr)
    castExpr.code = castExpr.code
    castExpr.address = castExpr.expr.address + " : " + castExpr.typ.toString
  }


  def genFieldExpr(fieldExpr: FieldExpr) = {
    genValueCode(fieldExpr.expr)
    fieldExpr.address = fieldExpr.expr.address + "." + fieldExpr.fieldId.name
    fieldExpr.code = fieldExpr.expr.code
  }
}