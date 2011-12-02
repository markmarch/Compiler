package edu.nyu.compiler

import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/16/11
 * Time: 3:59 PM
 */

trait IRGenerator extends TypeAnalyzer {
  private[this] var nextLabelIndex = 0
  private[this] var nextTempIndex = 0
  private[this] val relationOp = Set("==", "!=", ">=", ">", "<=", "<")
  private[this] val arithmeticOp = Set("+", "-", "*", "/", "%")

  private[this] def mkLabel() = {
    nextLabelIndex += 1
    "L" + nextLabelIndex
  }

  private[this] def mkTemp() = {
    nextTempIndex += 1
    "t" + nextTempIndex
  }

  /**
   * Generate IR code for the given program.
   */
  def generateIR(p: Program): List[String] = {
    analyzeType(p) match {
      case Left(e) => println(e); Nil
      case Right(_) => p.funList.flatMap(genFunDef)
    }
  }

  def genFunDef(f: FunDef) = {
    genStmt(f.blockStmt)
    f.id.name + " = fun" + f.typ.fromType.toString + " -> " + f.typ.returnType.toString :: f.blockStmt.code.map("  " + _)
  }

  def genStmt(stmt: Stmt) {
    stmt match {
      case v: VarDef => genVarDef(v)
      case a: AssignStmt => genAssignStmt(a)
      case b: BlockStmt => genBlockStmt(b)
      case c: CallStmt => genCallStmt(c)
      case f: ForStmt => genForStmt(f)
      case i: IfStmt => genIfStmt(i)
      case r: ReturnStmt => genReturnStmt(r)
      case w: WhileStmt => genWhileStmt(w)
    }
  }

  def genCallStmt(c: CallStmt) {
    genCallExpr(c.expr, true)
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
    val code = new ListBuffer[String]
    code.append(w.before + ":")
    code.appendAll(w.expr.code)
    code.append(w.expr.t + ":")
    genStmt(w.blockStmt)
    code.appendAll(w.blockStmt.code)
    code.append("goto " + w.before + ";")
    code.append(w.after + ":")
    w.code = code.toList
  }

  def genVarDef(v: VarDef) = genAssignment(v, v.varId, v.expr)

  def genAssignStmt(a: AssignStmt) = genAssignment(a, a.left, a.right)
  
  def genAssignment(s: Stmt, left: Expression, right: Expression) {
    left match {
      case s: SubscriptExpr => genSubscriptExpr(s, true)
      case _ => genValueCode(left)
    }
    right match {
      case t@InfixExpr("+", _, _) if right.typ == PrimitiveType("string") =>
        genStringAddExpr(t)
        s.code = left.code ::: t.code :::
          List(left.address + " = " + t.address + ";")
      case InfixExpr(op, l, r) if arithmeticOp.contains(op) =>
        genValueCode(l)
        genValueCode(r)
        s.code = left.code ::: l.code :::
          r.code ::: List(left.address + " = " + l.address + " " + op + " " + r.address + ";")
      case PrefixExpr("!", _) =>
        right.t = mkLabel()
        right.f = mkLabel()
        genJumpingCode(right)
        s.code = left.address + " = true;" :: right.code ::: List(right.f + ":", left.address + " = false;", right.t + ":")
      case PrefixExpr(op, e) =>
        genValueCode(e)
        s.code = left.code ::: e.code
        left match {
          case sub: SubscriptExpr =>
            right.address = mkTemp()
            s.code = s.code :::
              List(right.address + " = " + op + e.address + ";", left.address + " = " + right.address + ";")
          case _ => s.code = s.code ::: List(left.address + " = " + op + e.address + ";")
        }
      case c: CastExpr =>
        genValueCode(c)
        s.code = left.code ::: c.code :::
          List(left.address + " = " + c.address + ";")
      case _ =>
        genValueCode(right)
        s.code = left.code ::: right.code ::: List(left.address + " = " + right.address + ";")
    }
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
    code.append(begin + ":")
    code.append("if " + index + " >= " + size + " goto " + after + ";")
    code.append(f.varId.name + " = " + f.expr.address + "[" + index + "]" + ";")
    f.blockStmt.stmtList.foreach(genStmt)
    code.appendAll(f.blockStmt.stmtList.flatMap(_.code))
    code.append(index + " = " + index + " + 1;")
    code.append("goto " + begin + ";")
    code.append(after + ":")
    f.code = code.toList
  }

  def genJumpingCode(expr: Expression) {
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
      case ParenExpr(e) =>
        e.t = expr.t
        e.f = expr.f
        genJumpingCode(e)
        expr.code = e.code
      case BoolLit(true) =>
        expr.code = List("goto " + expr.t + ";")
      case BoolLit(false) =>
        expr.code = List("goto " + expr.f + ";")
      case _ =>
        genValueCode(expr)
        expr.code = expr.code ::: List("if " + expr.address + " goto " + expr.t + ";",
          "goto " + expr.f + ";")
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

  def genSubscriptExpr(subExpr: SubscriptExpr, returnRef: Boolean = false) {
    genValueCode(subExpr.left)
    genValueCode(subExpr.right)
    subExpr.code = subExpr.left.code ::: subExpr.right.code
    if (returnRef) {
      subExpr.address = subExpr.left.address + "[" + subExpr.right.address + "]"
    } else {
      subExpr.address = mkTemp()
      subExpr.code = subExpr.code ::: List(subExpr.address + " = " + subExpr.left.address + "[" + subExpr.right.address + "];")
    }
  }

  def genArrayLit(a: ArrayLit) = {
    val typ = a.typ.asInstanceOf[ArrayType]
    val code = new ListBuffer[String]
    a.exprList.foreach(genValueCode)

    code.appendAll(a.exprList.flatMap(_.code))

    val temp = mkTemp()
    code.append("param [0 : 2] = sizeof(" + typ.elementType.toString + ");")
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
    val typ = r.typ.asInstanceOf[RecordType]
    val fieldLitList = r.fieldLitList
    val temp = mkTemp()
    val code = new ListBuffer[String]
    fieldLitList.foreach(genValueCode)
    code.appendAll(fieldLitList.flatMap(_.code))
    code.append("param[0 : 1] = sizeof(" + typ.toString + ");")
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
    prefixExpr.address = mkTemp()
    prefixExpr.code = prefixExpr.expr.code ::: List(prefixExpr.address + " = " + prefixExpr.op + prefixExpr.expr.address + ";")
  }

  def genInfixExpr(i: InfixExpr) {
    i.op match {
      case op if op == "+" && i.typ == PrimitiveType("string") => genStringAddExpr(i)
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

  def genStringAddExpr(i: InfixExpr) {
    val (left, right) = (i.left, i.right)
    genValueCode(left)
    genValueCode(right)

    val code = new ListBuffer[String]
    code.appendAll(left.code)
    code.appendAll(right.code)
    code.append("param[0 : 2] = " + left.address + ";")
    code.append("param[1 : 2] = " + right.address + ";")
    i.address = mkTemp()
    code.append(i.address + " = call append : 2;")
    i.code = code.toList
  }

  def genCallExpr(expr: CallExpr, withReturnValue: Boolean = false) {
    val callee = expr.callee
    val params = expr.paramList
    val arity = params.size

    params.foreach(genValueCode)
    val prepareParamCode =
      for ((a, i) <- params.map(_.address).zipWithIndex)
      yield "param[" + i + " : " + arity + "] = " + a + ";"
    expr.code = params.flatMap(_.code) ::: prepareParamCode
    if (!withReturnValue) {
      expr.address = mkTemp()
      expr.code = expr.code ::: List(expr.address + " = call " + callee.name + " : " + arity + ";")
    } else {
      expr.address = "call " + callee.name + " : " + arity + ";"
    }
  }

  def genCastExpr(castExpr: CastExpr) {
    genValueCode(castExpr.expr)
    castExpr.address = mkTemp()
    castExpr.code = castExpr.expr.code :::
      List(castExpr.address + " = " + castExpr.expr.address + " : " + castExpr.toType.toString + ";")
  }


  def genFieldExpr(fieldExpr: FieldExpr) = {
    genValueCode(fieldExpr.expr)
    fieldExpr.address = mkTemp()
    fieldExpr.code = fieldExpr.expr.code ::: List(fieldExpr.address + " = " + fieldExpr.expr.address + "." + fieldExpr.fieldId.name + ";")
  }
}