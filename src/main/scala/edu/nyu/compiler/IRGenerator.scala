package edu.nyu.compiler

import collection.mutable.{ListBuffer,  HashMap}

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/16/11
 * Time: 3:59 PM
 */

trait IRGenerator extends TypeAnalyzer {
  private[this] val relationOp = Set("==", "!=", ">=", ">", "<=", "<")
  private[this] val arithmeticOp = Set("+", "-", "*", "/", "%")

  def findAddresses(addresses : HashMap[String, Address], scope : Scope): HashMap[String, Address] = {
    for(key <- scope.symbols.keySet) {
      val s = scope.symbols(key)
      s.definition match {
        case v : VarId => s.address = NameAddr(nextUniqueName(addresses, v.name))
        case f : FieldId => s.address = NameAddr(nextUniqueName(addresses, f.name))
        case f : FunId => s.address = NameAddr(nextUniqueName(addresses, f.name))
      }
    }
    for(s <- scope.children)
      findAddresses(addresses, s)
    addresses
  }

  private[this] def nextUniqueName(map : HashMap[String, Address], srcName : String) : String = {
    val result = if (map.contains(srcName)) {
      var i = 0
      while(map.contains(srcName + i))
        i += 1
      srcName + i
    } else srcName
    map.put(result, NameAddr(result))
    result
  }

  private[this] def mkLabel() = {
    val fun = symbolTable.currnetFuction
    nextUniqueName(fun.addresses, "L_" + fun.id.name)
  }

  private[this] def mkTemp() = {
    NameAddr(nextUniqueName(symbolTable.currnetFuction.addresses, "t"))
  }
  
  def generateIR(p: Program): List[String] = {
    analyzeType(p) match {
      case Left(e) => println(e); Nil
      case Right(_) => p.funList.flatMap(genFunDef)
    }
  }

  def genFunDef(f: FunDef) = {
    symbolTable.push(f.scope)
    symbolTable.currnetFuction = f
    f.addresses = findAddresses(HashMap.empty[String,  Address], f.scope)
    genStmt(f.blockStmt)
    symbolTable.pop()
    f.id.name + " = fun" + f.typ.fromType.toString + " -> " + f.typ.returnType.toString ::
      f.blockStmt.code.map(
        i => i match {
          case _: EmptyLabel => "  " + i.toString
          case _ => "  " + i.toString + ";"
        })
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
    c.code = c.expr.code
  }

  def genBlockStmt(b: BlockStmt) {
    if (b.scope != null) {
      symbolTable.push(b.scope)
    }
    b.stmtList.foreach(genStmt)
    b.code = b.stmtList.flatMap(_.code)
    if (b.scope != null) {
      symbolTable.pop()
    }
  }

  def genIfStmt(i: IfStmt) {
    i.expr.t = mkLabel()
    i.expr.f = mkLabel()
    genJumpingCode(i.expr)
    genStmt(i.blockStmt)

    val code = new ListBuffer[Instruction]
    code.appendAll(i.expr.code)
    code.append(UnconditionalJumpInstr(i.expr.f))
    code.append(EmptyLabel(i.expr.t))
    code.appendAll(i.blockStmt.code)
    i.optBlockStmt match {
      case Some(b) =>
        val after = mkLabel()
        code.append(UnconditionalJumpInstr(after))
        code.append(EmptyLabel(i.expr.f))
        genStmt(b)
        code.appendAll(b.code)
        code.append(EmptyLabel(after))
      case None => code.append(EmptyLabel(i.expr.f))
    }
    i.code = code.toList
  }

  def genReturnStmt(r: ReturnStmt) {
    r.optExpr match {
      case Some(e) =>
        genValueCode(e)
        r.code = e.code ::: List(ReturnInstr(Some(e.address)))
      case None => r.code = List(ReturnInstr(None))
    }
  }

  def genWhileStmt(w: WhileStmt) {
    w.before = mkLabel()
    w.after = mkLabel()
    w.expr.t = mkLabel()
    w.expr.f = w.after
    genJumpingCode(w.expr)
    val code = new ListBuffer[Instruction]
    code.append(EmptyLabel(w.before))
    code.appendAll(w.expr.code)
    code.append(EmptyLabel(w.expr.t))
    genStmt(w.blockStmt)
    code.appendAll(w.blockStmt.code)
    code.append(UnconditionalJumpInstr(w.before))
    code.append(EmptyLabel(w.after))
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
          List(CopyInstr(left.address, t.address))
      case InfixExpr(op, l, r) if arithmeticOp.contains(op) =>
        genValueCode(l)
        genValueCode(r)
        s.code = left.code ::: l.code :::
          r.code ::: List(InfixInstr(left.address, op, l.address, r.address))
      case PrefixExpr("!", _) =>
        right.t = mkLabel()
        right.f = mkLabel()
        genJumpingCode(right)
        s.code = CopyInstr(left.address, ConstAddr("true")) :: right.code :::
          List(LabeledInstruction(right.f, CopyInstr(left.address, ConstAddr("false"))), EmptyLabel(right.t))
      case PrefixExpr(op, e) =>
        genValueCode(e)
        s.code = left.code ::: e.code
        left match {
          case sub: SubscriptExpr =>
            right.address = mkTemp()
            s.code = s.code :::
              List(PrefixInstr(right.address, op, e.address), CopyInstr(left.address, right.address))
          case _ => s.code = s.code ::: List(PrefixInstr(left.address, op, e.address))
        }
      case c: CastExpr =>
        genValueCode(c)
        s.code = left.code ::: c.code :::
          List(CopyInstr(left.address, c.address))
      case _ =>
        genValueCode(right)
        s.code = left.code ::: right.code :::
          List(CopyInstr(left.address, right.address))
    }
  }

  def genForStmt(f: ForStmt) = {
    genValueCode(f.varId)
    genValueCode(f.expr)
    val code = new ListBuffer[Instruction]
    code.appendAll(f.expr.code)
    val size = mkTemp()
    val index = mkTemp()
    code.append(ParamInstr(0, 1, f.expr.address))
    code.append(CallInstr(Some(size), "size", 1))
    code.append(CopyInstr(index, ConstAddr("0")))
    val begin = mkLabel()
    val after = mkLabel()
    code.append(LabeledInstruction(begin, RelopJumpInstr(">=", index, size, after)))
    code.append(ArrayReadInstr(f.varId.address, f.expr.address, index))
    f.blockStmt.stmtList.foreach(genStmt)
    code.appendAll(f.blockStmt.stmtList.flatMap(_.code))
    code.append(InfixInstr(index, "+", index, ConstAddr("1")))
    code.append(UnconditionalJumpInstr(begin))
    code.append(EmptyLabel(after))
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
        expr.code = left.code ::: List(EmptyLabel(left.f)) ::: right.code
      case InfixExpr("&&", left, right) =>
        left.t = mkLabel()
        left.f = expr.f
        right.t = expr.t
        right.f = expr.f
        genJumpingCode(left)
        genJumpingCode(right)
        expr.code = left.code ::: List(EmptyLabel(left.t)) ::: right.code
      case InfixExpr(op, left, right) if relationOp.contains(op) =>
        genValueCode(left)
        genValueCode(right)
        expr.code = left.code ::: right.code :::
          List(RelopJumpInstr(op, left.address, right.address, expr.t), UnconditionalJumpInstr(expr.f))
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
        expr.code = List(UnconditionalJumpInstr(expr.t))
      case BoolLit(false) =>
        expr.code = List(UnconditionalJumpInstr(expr.f))
      case _ =>
        genValueCode(expr)
        expr.code = expr.code ::: List(TrueJumpInstr(expr.address, expr.t), UnconditionalJumpInstr(expr.f))
    }
  }

  def genValueCode(expr: Expression): Unit = expr match {
    case i: VarId => i.address = i.symbol.address
    case b: BoolLit => b.address = ConstAddr(b.value.toString)
    case s: StringLit => s.address = ConstAddr("\"" + s.value + "\"")
    case i: IntLit => i.address = ConstAddr(i.value.toString)
    case n: NullLit => n.address = ConstAddr("null")
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
      subExpr.address = NameAddr(subExpr.left.address + "[" + subExpr.right.address + "]")
    } else {
      subExpr.address = mkTemp()
      subExpr.code = subExpr.code ::: List(ArrayReadInstr(subExpr.address, subExpr.left.address, subExpr.right.address))
    }
  }

  def genArrayLit(a: ArrayLit) = {
    val typ = a.typ.asInstanceOf[ArrayType]
    val code = new ListBuffer[Instruction]
    a.exprList.foreach(genValueCode)

    code.appendAll(a.exprList.flatMap(_.code))

    val temp = mkTemp()
    code.append(ParamInstr(0, 2, SizeOfAddr(typ.elementType)))
    code.append(ParamInstr(1, 2, ConstAddr(a.exprList.size.toString)))
    code.append(CallInstr(Some(temp), "newArray", 2))

    val assignCode =
      for ((ele, i) <- a.exprList.map(_.address).zipWithIndex)
      yield ArrayWriteInstr(temp, ConstAddr(i.toString), ele)
    code.appendAll(assignCode)

    a.address = temp
    a.code = code.toList
  }

  def genRecordLit(r: RecordLit) {
    val typ = r.typ.asInstanceOf[RecordType]
    val fieldLitList = r.fieldLitList
    val temp = mkTemp()
    val code = new ListBuffer[Instruction]
    fieldLitList.foreach(genValueCode)
    code.appendAll(fieldLitList.flatMap(_.code))
    code.append(ParamInstr(0, 1, SizeOfAddr(typ)))
    code.append(CallInstr(Some(temp), "newRecord", 1))
    val assignCode =
      for ((field, value) <- typ.fieldTypeList.map(_.fieldId.name).zip(fieldLitList.map(_.address)))
      yield RecWriteInstr(temp, field, value)
    code.appendAll(assignCode)
    r.address = temp
    r.code = code.toList
  }

  def genFieldLit(f: FieldLit) {
    genValueCode(f.expr)
    f.address = f.expr.address
    f.code = f.expr.code
  }

  def genPrefixExpr(p: PrefixExpr) {
    genValueCode(p.expr)
    p.address = mkTemp()
    p.code = p.expr.code ::: List(PrefixInstr(p.address, p.op, p.expr.address))
  }

  def genInfixExpr(i: InfixExpr) {
    i.op match {
      case op if op == "+" && i.typ == PrimitiveType("string") => genStringAddExpr(i)
      case op if arithmeticOp.contains(op) =>
        genValueCode(i.left)
        genValueCode(i.right)
        i.address = mkTemp()
        i.code = i.left.code ::: i.right.code :::
          List(InfixInstr(i.address, i.op, i.left.address, i.right.address))
      case _ =>
        i.t = mkLabel()
        i.f = mkLabel()
        genJumpingCode(i)
        val jumpingCode = i.code
        i.address = mkTemp()
        val code = new ListBuffer[Instruction]
        code.append(CopyInstr(i.address, ConstAddr("true")))
        code.appendAll(jumpingCode)
        code.append(LabeledInstruction(i.f, CopyInstr(i.address, ConstAddr("false"))))
        code.append(EmptyLabel(i.t))
        i.code = code.toList
    }
  }

  def genStringAddExpr(i: InfixExpr) {
    val (left, right) = (i.left, i.right)
    genValueCode(left)
    genValueCode(right)

    val code = new ListBuffer[Instruction]
    code.appendAll(left.code)
    code.appendAll(right.code)
    code.append(ParamInstr(0, 2, i.left.address))
    code.append(ParamInstr(1, 2, i.right.address))
    i.address = mkTemp()
    code.append(CallInstr(Some(i.address), "append", 2))
    i.code = code.toList
  }

  def genCallExpr(expr: CallExpr, withReturnValue: Boolean = false) {
    val callee = expr.callee
    val params = expr.paramList
    val arity = params.size

    params.foreach(genValueCode)
    val prepareParamCode =
      for ((a, i) <- params.map(_.address).zipWithIndex)
      yield ParamInstr(i, arity, a)
    expr.code = params.flatMap(_.code) ::: prepareParamCode
    val optionAddr = if (withReturnValue) None
    else {
      expr.address = mkTemp()
      Some(expr.address)
    }
    expr.code = expr.code ::: List(CallInstr(optionAddr, callee.name, arity))
  }

  def genCastExpr(c: CastExpr) {
    genValueCode(c.expr)
    c.address = mkTemp()
    c.code = c.expr.code :::
      List(CastInstr(c.address, c.expr.address, c.toType))
  }


  def genFieldExpr(f: FieldExpr) = {
    genValueCode(f.expr)
    f.address = mkTemp()
    f.code = f.expr.code ::: List(RecReadInstr(f.address, f.expr.address, f.fieldId.name))
  }
}